{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.CursorType
  ( HasJsonCursorType(..)
  , JsonCursorType(..)
  , jsonCursorPos
  ) where

import qualified Data.ByteString                                            as BS
import           Data.Char
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

data JsonCursorType
  = JsonCursorArray
  | JsonCursorBool
  | JsonCursorNull
  | JsonCursorNumber
  | JsonCursorObject
  | JsonCursorString
  deriving (Eq, Show)

class HasJsonCursorType k where
  jsonCursorType :: k -> JsonCursorType

jsonCursorType' :: Char -> JsonCursorType
jsonCursorType' c = case c of
  '[' -> JsonCursorArray
  't' -> JsonCursorBool
  'f' -> JsonCursorBool
  '0' -> JsonCursorNumber
  '1' -> JsonCursorNumber
  '2' -> JsonCursorNumber
  '3' -> JsonCursorNumber
  '4' -> JsonCursorNumber
  '5' -> JsonCursorNumber
  '6' -> JsonCursorNumber
  '7' -> JsonCursorNumber
  '8' -> JsonCursorNumber
  '9' -> JsonCursorNumber
  '+' -> JsonCursorNumber
  '-' -> JsonCursorNumber
  'n' -> JsonCursorNull
  '{' -> JsonCursorObject
  '"' -> JsonCursorString
  _   -> error "Invalid JsonCursor cursorRank"

jsonCursorPos :: (Rank1 w, Select1 v, VectorLike s) => JsonCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k

jsonCursorElemAt :: (Rank1 w, Select1 v, VectorLike s) => JsonCursor s v w -> Elem s
jsonCursorElemAt k = cursorText k !!! jsonCursorPos k

instance (Rank1 i, Select1 i, Rank1 b) => HasJsonCursorType (JsonCursor String i b) where
  jsonCursorType = jsonCursorType' . jsonCursorElemAt

instance (Rank1 i, Select1 i, Rank1 b) => HasJsonCursorType (JsonCursor BS.ByteString i b) where
  jsonCursorType = jsonCursorType' . chr . fromIntegral . jsonCursorElemAt
