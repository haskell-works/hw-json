{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Internal.Backend.Standard.StateMachine
  ( phiTable
  , phiTableSimd
  , transitionTable
  , transitionTableSimd
  , State(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector                           as DV
import qualified Data.Vector.Storable                  as DVS
import qualified HaskellWorks.Data.Json.Internal.Word8 as W8

{-# ANN module ("HLint: ignore Redundant guard"  :: String) #-}

data State = InJson | InString | InEscape | InValue deriving (Eq, Enum, Bounded, Show)

phiTable :: DV.Vector (DVS.Vector Word8)
phiTable = DV.constructN 5 gos
  where gos :: DV.Vector (DVS.Vector Word8) -> DVS.Vector Word8
        gos v = DVS.constructN 256 go
          where vi = DV.length v
                go :: DVS.Vector Word8 -> Word8
                go u = fromIntegral (snd (stateMachine ui (toEnum vi)))
                  where ui = fromIntegral (DVS.length u)
{-# NOINLINE phiTable #-}

phiTableSimd :: DVS.Vector Word32
phiTableSimd = DVS.constructN 256 go
  where go :: DVS.Vector Word32 -> Word32
        go v =  (snd (stateMachine vi InJson  ) .<.  0) .|.
                (snd (stateMachine vi InString) .<.  8) .|.
                (snd (stateMachine vi InEscape) .<. 16) .|.
                (snd (stateMachine vi InValue ) .<. 24)
          where vi = fromIntegral (DVS.length v)
{-# NOINLINE phiTableSimd #-}

transitionTable :: DV.Vector (DVS.Vector Word8)
transitionTable = DV.constructN 5 gos
  where gos :: DV.Vector (DVS.Vector Word8) -> DVS.Vector Word8
        gos v = DVS.constructN 256 go
          where vi = DV.length v
                go :: DVS.Vector Word8 -> Word8
                go u = fromIntegral (fromEnum (fst (stateMachine ui (toEnum vi))))
                  where ui = fromIntegral (DVS.length u)
{-# NOINLINE transitionTable #-}

transitionTableSimd :: DVS.Vector Word64
transitionTableSimd = DVS.constructN 256 go
  where go :: DVS.Vector Word64 -> Word64
        go v =  fromIntegral (fromEnum (fst (stateMachine vi InJson  ))) .|.
                fromIntegral (fromEnum (fst (stateMachine vi InString))) .|.
                fromIntegral (fromEnum (fst (stateMachine vi InEscape))) .|.
                fromIntegral (fromEnum (fst (stateMachine vi InValue )))
          where vi = fromIntegral (DVS.length v)
{-# NOINLINE transitionTableSimd #-}

stateMachine :: Word8 -> State -> (State, Word32)
stateMachine c InJson   | W8.isOpen c         = (InJson  , 0b110)
stateMachine c InJson   | W8.isClose c        = (InJson  , 0b001)
stateMachine c InJson   | W8.isDelim c        = (InJson  , 0b000)
stateMachine c InJson   | W8.isValueChar c    = (InValue , 0b111)
stateMachine c InJson   | W8.isDoubleQuote c  = (InString, 0b111)
stateMachine _ InJson   | otherwise           = (InJson  , 0b000)
stateMachine c InString | W8.isDoubleQuote c  = (InJson  , 0b000)
stateMachine c InString | W8.isBackSlash c    = (InEscape, 0b000)
stateMachine _ InString | otherwise           = (InString, 0b000)
stateMachine _ InEscape | otherwise           = (InString, 0b000)
stateMachine c InValue  | W8.isOpen c         = (InJson  , 0b110)
stateMachine c InValue  | W8.isClose c        = (InJson  , 0b001)
stateMachine c InValue  | W8.isDelim c        = (InJson  , 0b000)
stateMachine c InValue  | W8.isValueChar c    = (InValue , 0b000)
stateMachine _ InValue  | otherwise           = (InJson  , 0b000)
{-# INLINE stateMachine #-}
