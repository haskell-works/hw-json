{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module HaskellWorks.Data.Json.Internal.MakeIndex
  ( blankedJsonToInterestBits
  , byteStringToBits
  , blankedJsonToBalancedParens
  , blankedJsonToBalancedParens2
  , compressWordAsBit
  , interestingWord8s
  ) where

import Control.Monad
import Data.Array.Unboxed             ((!))
import Data.ByteString                (ByteString)
import Data.Int
import Data.Word
import Data.Word8
import HaskellWorks.Data.Bits.BitWise
import Prelude                        as P

import qualified Data.Array.Unboxed as A
import qualified Data.Bits          as BITS
import qualified Data.ByteString    as BS

interestingWord8s :: A.UArray Word8 Word8
interestingWord8s = A.array (0, 255) [
  (w, if w == _bracketleft || w == _braceleft || w == _parenleft || w == _t || w == _f || w == _n || w == _1
    then 1
    else 0)
  | w <- [0 .. 255]]

blankedJsonToInterestBits :: [BS.ByteString] -> [BS.ByteString]
blankedJsonToInterestBits = blankedJsonToInterestBits' ""

padRight :: Word8 -> Int -> BS.ByteString -> BS.ByteString
padRight w n bs = if BS.length bs >= n then bs else fst (BS.unfoldrN n gen bs)
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen cs = case BS.uncons cs of
          Just (c, ds) -> Just (c, ds)
          Nothing      -> Just (w, BS.empty)

blankedJsonToInterestBits' :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
blankedJsonToInterestBits' rs as = case as of
  (bs:bss) ->
      let cs = if BS.length rs /= 0 then BS.concat [rs, bs] else bs in
      let lencs = BS.length cs in
      let q = lencs + 7 `quot` 8 in
      let (ds, es) = BS.splitAt (q * 8) cs in
      let (fs, _) = BS.unfoldrN q gen ds in
      fs:blankedJsonToInterestBits' es bss
  [] -> []
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen ds = if BS.length ds == 0
          then Nothing
          else Just ( BS.foldr (\b m -> (interestingWord8s ! b) .|. (m .<. 1)) 0 (padRight 0 8 (BS.take 8 ds))
                    , BS.drop 8 ds
                    )

blankedJsonToBalancedParens :: [BS.ByteString] -> [Bool]
blankedJsonToBalancedParens (as) = case as of
    (bs:bss) -> blankedJsonToBalancedParens' bs bss []
    []       -> []

blankedJsonToBalancedParens' :: BS.ByteString -> [BS.ByteString] -> ([Bool] -> [Bool])
blankedJsonToBalancedParens' bs = case BS.uncons bs of
  Just (c, cs) -> do
    let t = case c of
          d | d == _braceleft     -> (True:)
          d | d == _braceright    -> (False:)
          d | d == _bracketleft   -> (True:)
          d | d == _bracketright  -> (False:)
          d | d == _parenleft     -> (True:)
          d | d == _parenright    -> (False:)
          d | d == _t             -> (True:) . (False:)
          d | d == _f             -> (True:) . (False:)
          d | d == _1             -> (True:) . (False:)
          d | d == _n             -> (True:) . (False:)
          _ -> id
    (t .) . blankedJsonToBalancedParens' cs
  Nothing -> const id

repartitionMod8 :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
repartitionMod8 aBS bBS = (BS.take cLen abBS, BS.drop cLen abBS)
  where abBS = BS.concat [aBS, bBS]
        abLen = BS.length abBS
        cLen = (abLen `div` 8) * 8

compressWordAsBit :: [BS.ByteString] -> [BS.ByteString]
compressWordAsBit = compressWordAsBit' BS.empty

compressWordAsBit' :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
compressWordAsBit' aBS as = case as of
  (bBS:bBSs) ->
    let (cBS, dBS) = repartitionMod8 aBS bBS in
    let (cs, _) = BS.unfoldrN (BS.length cBS + 7 `div` 8) gen cBS in
    cs:compressWordAsBit' dBS bBSs
  [] -> do
    let (cs, _) = BS.unfoldrN (BS.length aBS + 7 `div` 8) gen aBS
    [cs]
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen xs = if BS.length xs == 0
          then Nothing
          else Just ( BS.foldr (\b m -> ((b .&. 1) .|. (m .<. 1))) 0 (padRight 0 8 (BS.take 8 xs))
                    , BS.drop 8 xs
                    )

blankedJsonToBalancedParens2 :: [BS.ByteString] -> [BS.ByteString]
blankedJsonToBalancedParens2 as = case as of
  (bs:bss) ->
    let (cs, _) = BS.unfoldrN (BS.length bs * 2) gen (Nothing, bs) in
    cs:blankedJsonToBalancedParens2 bss
  [] -> []
  where gen :: (Maybe Bool, ByteString) -> Maybe (Word8, (Maybe Bool, ByteString))
        gen (Just True  , bs) = Just (0xFF, (Nothing, bs))
        gen (Just False , bs) = Just (0x00, (Nothing, bs))
        gen (Nothing    , bs) = case BS.uncons bs of
          Just (c, cs) -> case balancedParensOf c of
            MiniN  -> gen         (Nothing    , cs)
            MiniT  -> Just (0xFF, (Nothing    , cs))
            MiniF  -> Just (0x00, (Nothing    , cs))
            MiniTF -> Just (0xFF, (Just False , cs))
          Nothing   -> Nothing

data MiniBP = MiniN | MiniT | MiniF | MiniTF

balancedParensOf :: Word8 -> MiniBP
balancedParensOf c = case c of
    d | d == _braceleft     -> MiniT
    d | d == _braceright    -> MiniF
    d | d == _bracketleft   -> MiniT
    d | d == _bracketright  -> MiniF
    d | d == _parenleft     -> MiniT
    d | d == _parenright    -> MiniF
    d | d == _t             -> MiniTF
    d | d == _f             -> MiniTF
    d | d == _1             -> MiniTF
    d | d == _n             -> MiniTF
    _ -> MiniN

yieldBitsOfWord8 :: Word8 -> [Bool] -> [Bool]
yieldBitsOfWord8 w =
  (((w .&. BITS.bit 0) /= 0):) .
  (((w .&. BITS.bit 1) /= 0):) .
  (((w .&. BITS.bit 2) /= 0):) .
  (((w .&. BITS.bit 3) /= 0):) .
  (((w .&. BITS.bit 4) /= 0):) .
  (((w .&. BITS.bit 5) /= 0):) .
  (((w .&. BITS.bit 6) /= 0):) .
  (((w .&. BITS.bit 7) /= 0):)

yieldBitsofWord8s :: [Word8] -> [Bool] -> [Bool]
yieldBitsofWord8s = P.foldr ((>>) . yieldBitsOfWord8) id

byteStringToBits :: [BS.ByteString] -> [Bool] -> [Bool]
byteStringToBits as = case as of
  (bs:bss) -> yieldBitsofWord8s (BS.unpack bs) . byteStringToBits bss
  []       -> id
