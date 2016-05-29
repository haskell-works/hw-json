module HaskellWorks.Data.Json.CharLike where

import Data.Word
import Data.Word8
import HaskellWorks.Data.Json.Conduit.Words

class JsonCharLike c where
  isLeadingDigit2 :: c -> Bool
  isQuotDbl :: c -> Bool
  isChar_t :: c -> Bool
  isChar_f :: c -> Bool
  isChar_n :: c -> Bool
  isBraceLeft :: c -> Bool
  isBracketLeft :: c -> Bool

instance JsonCharLike Word8 where
  isLeadingDigit2 = isLeadingDigit
  isQuotDbl       = (== _quotedbl)
  isChar_t        = (== _t)
  isChar_f        = (== _f)
  isChar_n        = (== _n)
  isBraceLeft     = (== _braceleft)
  isBracketLeft   = (== _bracketleft)
