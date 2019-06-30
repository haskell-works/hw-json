module HaskellWorks.Data.Json.Internal.Word64 where

import Data.Word

import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.Word8 as W8

doubleQuote :: Word64
doubleQuote = 0x0101010101010101 * fromIntegral W8.doubleQuote

backSlack :: Word64
backSlack = 0x0101010101010101 * fromIntegral W8.backSlash

openBrace :: Word64
openBrace = 0x0101010101010101 * fromIntegral W8.openBrace

closeBrace :: Word64
closeBrace = 0x0101010101010101 * fromIntegral W8.closeBrace

openBracket :: Word64
openBracket = 0x0101010101010101 * fromIntegral W8.openBracket

closeBracket :: Word64
closeBracket = 0x0101010101010101 * fromIntegral W8.closeBracket

comma :: Word64
comma = 0x0101010101010101 * fromIntegral W8.comma

colon :: Word64
colon = 0x0101010101010101 * fromIntegral W8.colon
