{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Json.Value where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8       as ABC
import qualified Data.Attoparsec.Types                  as T
import qualified Data.ByteString                        as BS
import           Data.Bits
import           Data.Char
import           Data.String
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Parser               as P

parseHexDigitNumeric :: P.Parser t => T.Parser t Int
parseHexDigitNumeric = do
  c <- satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

parseHexDigitAlphaLower :: P.Parser t => T.Parser t Int
parseHexDigitAlphaLower = do
  c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

parseHexDigitAlphaUpper :: P.Parser t => T.Parser t Int
parseHexDigitAlphaUpper = do
  c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

parseHexDigit :: P.Parser t => T.Parser t Int
parseHexDigit = parseHexDigitNumeric <|> parseHexDigitAlphaLower <|> parseHexDigitAlphaUpper

parseJsonString :: (P.Parser t, IsString t) => T.Parser t String
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
      return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

data JsonValue
  = JsonString String
  | JsonNumber Double
  | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

class JsonValueAt a where
  jsonValueAt :: a -> Either DecodeError JsonValue

instance JsonValueAt JsonIndex where
  jsonValueAt i = case i of
    JsonIndexString  s  -> case ABC.parse parseJsonString s of
      ABC.Fail    {}    -> Left (DecodeError ("Invalid string: '" ++ show (BS.take 20 s) ++ "...'"))
      ABC.Partial _     -> Left (DecodeError "Unexpected end of string")
      ABC.Done    _ r   -> Right (JsonString r)
    JsonIndexNumber  s  -> case ABC.parse ABC.rational s of
      ABC.Fail    {}    -> Left (DecodeError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'"))
      ABC.Partial f     -> case f " " of
        ABC.Fail    {}    -> Left (DecodeError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'"))
        ABC.Partial _     -> Left (DecodeError "Unexpected end of number")
        ABC.Done    _ r   -> Right (JsonNumber r)
      ABC.Done    _ r   -> Right (JsonNumber r)
    JsonIndexObject  fs -> JsonObject <$> mapM (\f -> (,) <$> parseString (fst f) <*> jsonValueAt (snd f)) fs
    JsonIndexArray   es -> JsonArray <$> mapM jsonValueAt es
    JsonIndexBool    v  -> Right (JsonBool v)
    JsonIndexNull       -> Right JsonNull
    where parseString bs = case ABC.parse parseJsonString bs of
            ABC.Fail    {}  -> Left (DecodeError ("Invalid field: '" ++ show (BS.take 20 bs) ++ "...'"))
            ABC.Partial _   -> Left (DecodeError "Unexpected end of field")
            ABC.Done    _ s -> Right s
