{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.Data.Json.Internal.Standard.Token.Tokenize
    ( IsChar(..)
    , JsonToken(..)
    , ParseJson(..)
    ) where

import Control.Applicative
import Data.Bits
import Data.Char
import Data.Word
import Data.Word8
import HaskellWorks.Data.Char.IsChar
import HaskellWorks.Data.Json.Internal.Token.Types
import HaskellWorks.Data.Parser                    ((<?>))

import qualified Data.Attoparsec.ByteString.Char8 as BC
import qualified Data.Attoparsec.Combinator       as AC
import qualified Data.Attoparsec.Types            as T
import qualified Data.ByteString                  as BS
import qualified HaskellWorks.Data.Parser         as P

{- HLINT ignore "Reduce duplication" -}

hexDigitNumeric :: P.Parser t u => T.Parser t Int
hexDigitNumeric = do
  c <- P.satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

hexDigitAlphaLower :: P.Parser t u => T.Parser t Int
hexDigitAlphaLower = do
  c <- P.satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

hexDigitAlphaUpper :: P.Parser t u => T.Parser t Int
hexDigitAlphaUpper = do
  c <- P.satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

hexDigit :: P.Parser t u => T.Parser t Int
hexDigit = hexDigitNumeric <|> hexDigitAlphaLower <|> hexDigitAlphaUpper

class ParseJson t s d where
  parseJsonTokenString :: T.Parser t (JsonToken s d)
  parseJsonToken :: T.Parser t (JsonToken s d)
  parseJsonTokenBraceL :: T.Parser t (JsonToken s d)
  parseJsonTokenBraceR :: T.Parser t (JsonToken s d)
  parseJsonTokenBracketL :: T.Parser t (JsonToken s d)
  parseJsonTokenBracketR :: T.Parser t (JsonToken s d)
  parseJsonTokenComma :: T.Parser t (JsonToken s d)
  parseJsonTokenColon :: T.Parser t (JsonToken s d)
  parseJsonTokenWhitespace :: T.Parser t (JsonToken s d)
  parseJsonTokenNull :: T.Parser t (JsonToken s d)
  parseJsonTokenBoolean :: T.Parser t (JsonToken s d)
  parseJsonTokenDouble :: T.Parser t (JsonToken s d)

  parseJsonToken =
    parseJsonTokenString     <|>
    parseJsonTokenBraceL     <|>
    parseJsonTokenBraceR     <|>
    parseJsonTokenBracketL   <|>
    parseJsonTokenBracketR   <|>
    parseJsonTokenComma      <|>
    parseJsonTokenColon      <|>
    parseJsonTokenWhitespace <|>
    parseJsonTokenNull       <|>
    parseJsonTokenBoolean    <|>
    parseJsonTokenDouble

instance ParseJson BS.ByteString String Double where
  parseJsonTokenBraceL    = P.string "{" >> return JsonTokenBraceL
  parseJsonTokenBraceR    = P.string "}" >> return JsonTokenBraceR
  parseJsonTokenBracketL  = P.string "[" >> return JsonTokenBracketL
  parseJsonTokenBracketR  = P.string "]" >> return JsonTokenBracketR
  parseJsonTokenComma     = P.string "," >> return JsonTokenComma
  parseJsonTokenColon     = P.string ":" >> return JsonTokenColon
  parseJsonTokenNull      = P.string "null" >> return JsonTokenNull
  parseJsonTokenDouble    = JsonTokenNumber <$> P.rational

  parseJsonTokenString = do
    _ <- P.string "\""
    value <- many (verbatimChar <|> escapedChar <|> escapedCode)
    _ <- P.string "\""
    return $ JsonTokenString value
    where
      verbatimChar  = P.satisfyChar (BC.notInClass "\"\\") <?> "invalid string character"
      escapedChar   = do
        _ <- P.string "\\"
        (   P.char '"'  >> return '"'  ) <|>
          ( P.char 'b'  >> return '\b' ) <|>
          ( P.char 'n'  >> return '\n' ) <|>
          ( P.char 'f'  >> return '\f' ) <|>
          ( P.char 'r'  >> return '\r' ) <|>
          ( P.char 't'  >> return '\t' ) <|>
          ( P.char '\\' >> return '\\' ) <|>
          ( P.char '\'' >> return '\'' ) <|>
          ( P.char '/'  >> return '/'  )
      escapedCode   = do
        _ <- P.string "\\u"
        a <- hexDigit
        b <- hexDigit
        c <- hexDigit
        d <- hexDigit
        return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

  parseJsonTokenWhitespace = do
    _ <- AC.many1' $ BC.choice [P.string " ", P.string "\t", P.string "\n", P.string "\r"]
    return JsonTokenWhitespace

  parseJsonTokenBoolean = true <|> false
    where true  = P.string "true"   >> return (JsonTokenBoolean True)
          false = P.string "false"  >> return (JsonTokenBoolean False)

instance ParseJson BS.ByteString BS.ByteString Double where
  parseJsonTokenBraceL    = P.string "{" >> return JsonTokenBraceL
  parseJsonTokenBraceR    = P.string "}" >> return JsonTokenBraceR
  parseJsonTokenBracketL  = P.string "[" >> return JsonTokenBracketL
  parseJsonTokenBracketR  = P.string "]" >> return JsonTokenBracketR
  parseJsonTokenComma     = P.string "," >> return JsonTokenComma
  parseJsonTokenColon     = P.string ":" >> return JsonTokenColon
  parseJsonTokenNull      = P.string "null" >> return JsonTokenNull
  parseJsonTokenDouble    = JsonTokenNumber <$> P.rational

  parseJsonTokenString = do
    _ <- P.string "\""
    value <- many (verbatimChar <|> escapedChar <|> escapedCode)
    _ <- P.string "\""
    return $ JsonTokenString $ BS.pack value
    where
      word :: Word8 -> T.Parser BS.ByteString Word8
      word w = P.satisfy (== w)
      verbatimChar :: T.Parser BS.ByteString Word8
      verbatimChar  = P.satisfy (\w -> w /= _quotedbl && w /= _backslash) -- <?> "invalid string character"
      escapedChar :: T.Parser BS.ByteString Word8
      escapedChar   = do
        _ <- P.string "\\"
        (   word _quotedbl    >> return _quotedbl       ) <|>
          ( word _b           >> return 0x08            ) <|>
          ( word _n           >> return _lf             ) <|>
          ( word _f           >> return _np             ) <|>
          ( word _r           >> return _cr             ) <|>
          ( word _t           >> return _tab            ) <|>
          ( word _backslash   >> return _backslash      ) <|>
          ( word _quotesingle >> return _quotesingle    ) <|>
          ( word _slash       >> return _slash          )
      escapedCode :: T.Parser BS.ByteString Word8
      escapedCode   = do
        _ <- P.string "\\u"
        a <- hexDigit
        b <- hexDigit
        c <- hexDigit
        d <- hexDigit
        return $ fromIntegral $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

  parseJsonTokenWhitespace = do
    _ <- AC.many1' $ BC.choice [P.string " ", P.string "\t", P.string "\n", P.string "\r"]
    return JsonTokenWhitespace

  parseJsonTokenBoolean = true <|> false
    where
      true  = P.string "true"   >> return (JsonTokenBoolean True)
      false = P.string "false"  >> return (JsonTokenBoolean False)
