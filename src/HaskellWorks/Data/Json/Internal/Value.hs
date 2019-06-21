{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Internal.Value where

import Control.Applicative
import Data.Aeson.Parser.Internal (jstring)
import Data.Char
import Data.Text                  (Text)
import HaskellWorks.Data.Parser   as P

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

parseJsonText :: ABC.Parser Text
parseJsonText = jstring
