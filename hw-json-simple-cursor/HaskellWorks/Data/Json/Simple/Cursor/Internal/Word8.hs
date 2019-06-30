module HaskellWorks.Data.Json.Simple.Cursor.Internal.Word8 where

import Data.Word
import Data.Word8 hiding (isDigit)

import qualified Data.Char as C

isLeadingDigit :: Word8 -> Bool
isLeadingDigit w = w == _hyphen || (w >= _0 && w <= _9)

isTrailingDigit :: Word8 -> Bool
isTrailingDigit w = w == _plus || w == _hyphen || (w >= _0 && w <= _9) || w == _period || w == _E || w == _e

isAlphabetic :: Word8 -> Bool
isAlphabetic w = (w >= _A && w <= _Z) || (w >= _a && w <= _z)

isDigit :: Word8 -> Bool
isDigit w = w >= _0 && w <= _9

wIsJsonNumberDigit :: Word8 -> Bool
wIsJsonNumberDigit w = (w >= _0 && w <= _9) || w == _hyphen

doubleQuote :: Word8
doubleQuote = fromIntegral (C.ord '"')

backSlash :: Word8
backSlash = fromIntegral (C.ord '\\')

openBrace :: Word8
openBrace = fromIntegral (C.ord '{')

closeBrace :: Word8
closeBrace = fromIntegral (C.ord '}')

openBracket :: Word8
openBracket = fromIntegral (C.ord '[')

closeBracket :: Word8
closeBracket = fromIntegral (C.ord ']')

comma :: Word8
comma = fromIntegral (C.ord ',')

colon :: Word8
colon = fromIntegral (C.ord ':')

isPeriod :: Word8 -> Bool
isPeriod w = w == 46

isMinus :: Word8 -> Bool
isMinus w = w == 45

isPlus :: Word8 -> Bool
isPlus w = w == 43

isValueChar :: Word8 -> Bool
isValueChar c = isAlphabetic c || isDigit c || isPeriod c || isMinus c || isPlus c

isOpen :: Word8 -> Bool
isOpen c = c == openBracket || c == openBrace

isClose :: Word8 -> Bool
isClose c = c == closeBracket || c == closeBrace

isDelim :: Word8 -> Bool
isDelim c = c == comma || c == colon

isDoubleQuote :: Word8 -> Bool
isDoubleQuote c = c == doubleQuote

isBackSlash :: Word8 -> Bool
isBackSlash c = c == backSlash
