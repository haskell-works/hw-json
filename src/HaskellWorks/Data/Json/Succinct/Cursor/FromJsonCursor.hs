{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module HaskellWorks.Data.Json.Succinct.Cursor.FromJsonCursor where

import qualified Data.ByteString as BS
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal
import           HaskellWorks.Data.Json.Succinct.Cursor.Token
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Json.Token.Types
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.TreeCursor

class FromJsonCursor t v w a where
  fromJsonCursor :: JsonCursor t v w -> a

instance (TreeCursor (JsonCursor BS.ByteString v w), Rank1 w, Select1 v, TestBit w) => FromJsonCursor BS.ByteString v w (GenJsonValue BS.ByteString BS.ByteString) where
  fromJsonCursor k = case jsonTokenAt k of
    Just JsonTokenBraceL          -> undefined
    Just JsonTokenBraceR          -> undefined
    Just JsonTokenBracketL        -> undefined
    Just JsonTokenBracketR        -> undefined
    Just JsonTokenComma           -> undefined
    Just JsonTokenColon           -> undefined
    Just JsonTokenWhitespace      -> undefined
    Just (JsonTokenString _)      -> undefined
    Just (JsonTokenBoolean value) -> JsonBool value
    Just (JsonTokenNumber _)      -> undefined
    Just JsonTokenNull            -> undefined
    Nothing                       -> undefined
