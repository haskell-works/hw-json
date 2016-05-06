module HaskellWorks.Data.Json.Token.Types (JsonToken(..)) where

data JsonToken s d
  = JsonTokenBraceL
  | JsonTokenBraceR
  | JsonTokenBracketL
  | JsonTokenBracketR
  | JsonTokenComma
  | JsonTokenColon
  | JsonTokenWhitespace
  | JsonTokenString s
  | JsonTokenBoolean Bool
  | JsonTokenNumber d
  | JsonTokenNull
  deriving (Eq, Show)
