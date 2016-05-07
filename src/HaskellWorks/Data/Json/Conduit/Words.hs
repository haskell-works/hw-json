module HaskellWorks.Data.Json.Conduit.Words where

import           Data.Word
import           Data.Word8

isLeadingDigit :: Word8 -> Bool
isLeadingDigit w = w == _hyphen || (w >= _0 && w <= _9)

isTrailingDigit :: Word8 -> Bool
isTrailingDigit w = w == _plus || w == _hyphen || (w >= _0 && w <= _9) || w == _period || w == _E || w == _e

isAlphabetic :: Word8 -> Bool
isAlphabetic w = (w >= _A && w <= _Z) || (w >= _a && w <= _z)
