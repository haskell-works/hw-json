{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Json.Backend.Standard.SemiIndex
  ( semiIndexBuilder
  , SemiIndex(..)
  , PreSiChunk(..)
  , SiChunk(..)
  , buildSemiIndex
  , State(..)
  , buildFromByteString2
  , buildFromByteString3
  , toIbBpBuilders
  ) where

import Control.Monad.ST
import Data.Bits.Pdep
import Data.Bits.Pext
import Data.Word
import Foreign.Storable                                              (Storable (..))
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Json.Internal.Backend.Standard.StateMachine (IntState (..), State (..))
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString                                                     as BS
import qualified Data.ByteString.Builder                                             as B
import qualified Data.ByteString.Lazy                                                as LBS
import qualified Data.ByteString.Unsafe                                              as BSU
import qualified Data.Vector.Storable                                                as DVS
import qualified Data.Vector.Storable.Mutable                                        as DVSM
import qualified HaskellWorks.Data.Bits.Writer.Storable                              as W
import qualified HaskellWorks.Data.ByteString                                        as BS
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.Blank              as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex          as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.StateMachine       as SM
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.ToBalancedParens64 as J
import qualified HaskellWorks.Data.Json.Internal.Word8                               as W8

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data PreSiChunk v = PreSiChunk
  { preSiChunkIb   :: !v -- interest bits
  , preSiChunkBpOp :: !v -- balanced parens interest bits
  , preSiChunkBpCl :: !v -- balanced parens open close
  } deriving (Functor, Traversable, Foldable)

data SiChunk v = SiChunk
  { siChunkIb :: !v -- interest bits
  , siChunkBp :: !v -- balanced parens open close
  } deriving (Functor, Traversable, Foldable)

data SemiIndex v = SemiIndex
  { semiIndexIb :: !v
  , semiIndexBp :: !v
  } deriving (Functor, Traversable, Foldable)

semiIndexBuilder :: LBS.ByteString -> SemiIndex B.Builder
semiIndexBuilder lbs = SemiIndex (B.lazyByteString ibs) (B.byteString (BS.toByteString bps))
  where blankedJson = J.blankJson (LBS.toChunks lbs)
        ibs = LBS.fromChunks (J.blankedJsonToInterestBits blankedJson)
        bps = J.toBalancedParens64 (J.BlankedJson blankedJson)
{-# INLINE semiIndexBuilder #-}

buildSemiIndex :: BS.ByteString -> SemiIndex (DVS.Vector Word64)
buildSemiIndex bs = DVS.createT $ do
  let len = (BS.length bs + 7) `div` 8
  mib <- W.newWriter len
  mbp <- W.newWriter (len * 2)
  buildFromByteString mib mbp bs 0 InJson
{-# INLINE buildSemiIndex #-}

buildFromByteString :: W.Writer s -> W.Writer s -> BS.ByteString -> Int -> State -> ST s (SemiIndex (DVS.MVector s Word64))
buildFromByteString ib bp bs i = go
  where go state = if i < BS.length bs
          then do
            let c = BSU.unsafeIndex bs i
            case state of
              InJson -> if
                | c == W8.openBracket || c == W8.openBrace -> do
                  W.unsafeWriteBit ib 1
                  W.unsafeWriteBit bp 1
                  buildFromByteString ib bp bs (i + 1) InJson
                | c == W8.closeBracket || c == W8.closeBrace -> do
                  W.unsafeWriteBit bp 0
                  W.unsafeWriteBit ib 0
                  buildFromByteString ib bp bs (i + 1) InJson
                | c == W8.comma || c == W8.colon -> do
                  W.unsafeWriteBit ib 0
                  buildFromByteString ib bp bs (i + 1) InJson
                | W8.isAlphabetic c || W8.isDigit c || W8.isPeriod c || W8.isMinus c || W8.isPlus c -> do
                  W.unsafeWriteBit ib 1
                  W.unsafeWriteBit bp 1
                  W.unsafeWriteBit bp 0
                  buildFromByteString ib bp bs (i + 1) InValue
                | c == W8.doubleQuote -> do
                  W.unsafeWriteBit ib 1
                  W.unsafeWriteBit bp 1
                  W.unsafeWriteBit bp 0
                  buildFromByteString ib bp bs (i + 1) InString
                | otherwise -> do
                  W.unsafeWriteBit ib 0
                  buildFromByteString ib bp bs (i + 1) InJson
              InString -> do
                W.unsafeWriteBit ib 0
                let newContext = if
                      | c == W8.doubleQuote -> InJson
                      | c == W8.backSlash   -> InEscape
                      | otherwise           -> InString
                buildFromByteString ib bp bs (i + 1) newContext
              InEscape -> do
                W.unsafeWriteBit ib 0
                buildFromByteString ib bp bs (i + 1) InString
              InValue -> if
                | W8.isAlphabetic c || W8.isDigit c || W8.isPeriod c || W8.isMinus c || W8.isPlus c -> do
                  W.unsafeWriteBit ib 0
                  buildFromByteString ib bp bs (i + 1) InValue
                | otherwise -> go InJson
          else do
            ibv <- W.written ib
            bpv <- W.written bp
            return (SemiIndex ibv bpv)
{-# INLINE buildFromByteString #-}

constructSI :: forall a s. Storable a => Int -> (Int -> s -> (s, a)) -> s -> (s, DVS.Vector a)
constructSI n f state = DVS.createT $ do
  mv <- DVSM.unsafeNew n
  state' <- go 0 state mv
  return (state', mv)
  where go :: Int -> s -> DVSM.MVector t a -> ST t s
        go i s mv = if i < DVSM.length mv
          then do
            let (s', a) = f i s
            DVSM.unsafeWrite mv i a
            go (i + 1) s' mv
          else return s
{-# INLINE constructSI #-}

buildFromByteString2 :: [BS.ByteString] -> [DVS.Vector Word64]
buildFromByteString2 = go (IntState (fromEnum InJson))
  where go :: IntState -> [BS.ByteString] -> [DVS.Vector Word64]
        go s (bs:bss) = v:go s' bss
          where (s', v) = constructSI (BS.length bs `div` 16) f s -- TODO adjust length
                f :: Int -> IntState -> (IntState, Word64)
                f i s'' = let j = i * 16 in transition16 s''
                  (BSU.unsafeIndex bs  j      )
                  (BSU.unsafeIndex bs (j +  1))
                  (BSU.unsafeIndex bs (j +  2))
                  (BSU.unsafeIndex bs (j +  3))
                  (BSU.unsafeIndex bs (j +  4))
                  (BSU.unsafeIndex bs (j +  5))
                  (BSU.unsafeIndex bs (j +  6))
                  (BSU.unsafeIndex bs (j +  7))
                  (BSU.unsafeIndex bs (j +  8))
                  (BSU.unsafeIndex bs (j +  9))
                  (BSU.unsafeIndex bs (j + 10))
                  (BSU.unsafeIndex bs (j + 11))
                  (BSU.unsafeIndex bs (j + 12))
                  (BSU.unsafeIndex bs (j + 13))
                  (BSU.unsafeIndex bs (j + 14))
                  (BSU.unsafeIndex bs (j + 15))
        go _ [] = []
{-# INLINE buildFromByteString2 #-}

transition16 :: IntState
  -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
  -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
  -> (IntState, Word64)
transition16 s  a b c d  e f g h  i j k l  m n o p = (sp, w)
  where (sa, wa) = transition1 s  a
        (sb, wb) = transition1 sa b
        (sc, wc) = transition1 sb c
        (sd, wd) = transition1 sc d
        (se, we) = transition1 sd e
        (sf, wf) = transition1 se f
        (sg, wg) = transition1 sf g
        (sh, wh) = transition1 sg h
        (si, wi) = transition1 sh i
        (sj, wj) = transition1 si j
        (sk, wk) = transition1 sj k
        (sl, wl) = transition1 sk l
        (sm, wm) = transition1 sl m
        (sn, wn) = transition1 sm n
        (so, wo) = transition1 sn o
        (sp, wp) = transition1 so p
        w = (fromIntegral wa       ) .|.
            (fromIntegral wb .<.  4) .|.
            (fromIntegral wc .<.  8) .|.
            (fromIntegral wd .<. 12) .|.
            (fromIntegral we .<. 16) .|.
            (fromIntegral wf .<. 20) .|.
            (fromIntegral wg .<. 24) .|.
            (fromIntegral wh .<. 28) .|.
            (fromIntegral wi .<. 32) .|.
            (fromIntegral wj .<. 36) .|.
            (fromIntegral wk .<. 40) .|.
            (fromIntegral wl .<. 44) .|.
            (fromIntegral wm .<. 48) .|.
            (fromIntegral wn .<. 52) .|.
            (fromIntegral wo .<. 56) .|.
            (fromIntegral wp .<. 60)
{-# INLINE transition16 #-}

transition1 :: IntState -> Word8 -> (IntState, Word64)
transition1 s a = let x = (s', w) in
  -- let !_ = trace ("--> " <> show (chr (fromIntegral a)) <> " " <> bitShow (fromIntegral w :: Word8)) x in
  x
  where s' =                SM.lookupTransitionTable s (fromIntegral a)
        w  = fromIntegral $ SM.lookupPhiTable        s (fromIntegral a)
{-# INLINE transition1 #-}

buildFromByteString3 :: [BS.ByteString] -> [PreSiChunk (DVS.Vector Word64)]
buildFromByteString3 bss = makePreSiChunk <$> buildFromByteString2 bss
  where makePreSiChunk :: DVS.Vector Word64 -> PreSiChunk (DVS.Vector Word64)
        makePreSiChunk v = PreSiChunk (makeIb v) (makeBpOp v) (makeBpCl v)
        makeIb :: DVS.Vector Word64 -> DVS.Vector Word64
        makeIb v = asVector64 $ BS.toByteString $ DVS.constructN (DVS.length v) go
          where go :: DVS.Vector Word16 -> Word16
                go u = let ui = DVS.length u in fromIntegral (pext (DVS.unsafeIndex v ui) 0x4444444444444444)
        makeBpOp :: DVS.Vector Word64 -> DVS.Vector Word64
        makeBpOp v = asVector64 $ BS.toByteString $ DVS.constructN (DVS.length v) go
          where go :: DVS.Vector Word16 -> Word16
                go u = let ui = DVS.length u in fromIntegral (pext (DVS.unsafeIndex v ui) 0x2222222222222222)
        makeBpCl v = asVector64 $ BS.toByteString $ DVS.constructN (DVS.length v) go
          where go :: DVS.Vector Word16 -> Word16
                go u = let ui = DVS.length u in fromIntegral (pext (DVS.unsafeIndex v ui) 0x1111111111111111)

toIbBpBuilders :: [PreSiChunk (DVS.Vector Word64)] -> [SiChunk (DVS.Vector Word64)]
toIbBpBuilders = go 0 0
  where go :: Word64 -> Word64 -> [PreSiChunk (DVS.Vector Word64)] -> [SiChunk (DVS.Vector Word64)]
        go b n (PreSiChunk ib bpOp bpCl:cs) = SiChunk ib bp: go b' n' cs
          where ((b', n'), bp) = mkBp b n bpOp bpCl
        go b _ [] = [SiChunk DVS.empty (DVS.singleton b)]
        mkBp :: Word64 -> Word64 -> DVS.Vector Word64 -> DVS.Vector Word64 -> ((Word64, Word64), DVS.Vector Word64)
        mkBp b n bpOp bpCl = DVS.createT $ do
          mv <- DVSM.unsafeNew (DVS.length bpOp)
          mkBpGo b n bpOp bpCl mv 0 0
        mkBpGo :: Word64 -> Word64 -> DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.MVector s Word64 -> Int -> Int -> ST s ((Word64, Word64), DVS.MVector s Word64)
        mkBpGo b n bpOp bpCl mv vi mvi = if vi < DVS.length bpOp
          then do
            let op    = DVS.unsafeIndex bpOp vi
            let cl    = DVS.unsafeIndex bpCl vi
            let (source, mask) = compress (op .&. 0xffffffff) (cl .&. 0xffffffff) (op .>. 8) (cl .>. 8)
            let wb = pext source mask
            let wn = popCount1 wb
            let tb = (wb .<. n) .|. b
            let tn = n + wn
            if tn < 64
              then do
                mkBpGo tb tn bpOp bpCl mv (vi + 1) mvi
              else do
                DVSM.unsafeWrite mv mvi tb
                let ub = wb .>. (64 - tn)
                let un = tn - 64
                mkBpGo ub un bpOp bpCl mv (vi + 1) (mvi + 1)
          else return ((b, n), DVSM.take mvi mv)
        compress :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
        compress oplo cllo ophi clhi = let x = (sw :: Word64, mw :: Word64) in
          -- let !_ = trace ("----> oplo: " <> bitShow oplo) oplo in
          -- let !_ = trace ("----> cllo: " <> bitShow cllo) cllo in
          -- let !_ = trace ("----> ophi: " <> bitShow ophi) ophi in
          -- let !_ = trace ("----> clhi: " <> bitShow clhi) clhi in

          -- let !_ = trace ("----> mlo:  " <> bitShow mlo ) mlo  in
          -- let !_ = trace ("----> mhi:  " <> bitShow mhi ) mhi  in
          -- let !_ = trace ("----> slo:  " <> bitShow slo ) slo  in
          -- let !_ = trace ("----> shi:  " <> bitShow shi ) shi  in
          -- let !_ = trace ("----> rlo:  " <> bitShow rlo ) rlo  in
          -- let !_ = trace ("----> rhi:  " <> bitShow rhi ) rhi  in
          -- let !_ = trace ("----> plo:  " <> bitShow plo ) plo  in
          -- let !_ = trace ("----> phi:  " <> bitShow phi ) phi  in
          -- let !_ = trace ("----> sw:   " <> bitShow sw  ) sw   in
          -- let !_ = trace ("----> mw:   " <> bitShow mw  ) mw   in
          x
          where mlo = pdep oplo 0x5555555555555555 .|. pdep       cllo  0xaaaaaaaaaaaaaaaa
                mhi = pdep ophi 0x5555555555555555 .|. pdep       clhi  0xaaaaaaaaaaaaaaaa
                slo = pdep oplo 0x5555555555555555
                shi = pdep ophi 0x5555555555555555
                rlo = pext slo mlo
                rhi = pext shi mhi
                plo = popCount1 mlo
                phi = popCount1 mhi
                sw  = rlo .|. rhi .<. plo
                mw  = (1 .<. (plo + phi)) - 1
