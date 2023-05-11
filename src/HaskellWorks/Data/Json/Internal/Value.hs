{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Internal.Value where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.String
import HaskellWorks.Data.Parser as P

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.Attoparsec.Types            as T

parseHexDigitNumeric :: P.Parser t u => T.Parser t Int
parseHexDigitNumeric = do
  c <- satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

parseHexDigitAlphaLower :: P.Parser t u => T.Parser t Int
parseHexDigitAlphaLower = do
  c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

parseHexDigitAlphaUpper :: P.Parser t u => T.Parser t Int
parseHexDigitAlphaUpper = do
  c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

parseHexDigit :: P.Parser t u => T.Parser t Int
parseHexDigit = parseHexDigitNumeric <|> parseHexDigitAlphaLower <|> parseHexDigitAlphaUpper

parseJsonString :: (P.Parser t u, IsString t) => T.Parser t String
parseJsonString = do
  _ <- string "\""
  value <- many (verbatimChar <|> escapedChar <|> escapedCode)
  _ <- string "\""
  return value
  where
    verbatimChar  = satisfyChar (ABC.notInClass "\"\\") <?> "invalid string character"
    escapedChar   = do
      _ <- string "\\"
      (   char '"'  >> return '"'  ) <|>
        ( char 'b'  >> return '\b' ) <|>
        ( char 'n'  >> return '\n' ) <|>
        ( char 'f'  >> return '\f' ) <|>
        ( char 'r'  >> return '\r' ) <|>
        ( char 't'  >> return '\t' ) <|>
        ( char '\\' >> return '\\' ) <|>
        ( char '\'' >> return '\'' ) <|>
        ( char '/'  >> return '/'  )
    escapedCode   = do
      _ <- string "\\u"
      a <- parseHexDigit
      b <- parseHexDigit
      c <- parseHexDigit
      d <- parseHexDigit
      let res =  a `shift` 12 .|. b `shift` 8 .|. c `shift` 4 .|. d
      return $ if res <= 0x10FFFF
                then chr res
                else '�'