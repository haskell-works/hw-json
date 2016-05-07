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
  jsonCursorType :: k -> Maybe JsonCursorType

jsonCursorType' :: Char -> Maybe JsonCursorType
jsonCursorType' c = case c of
  '[' -> Just JsonCursorArray
  't' -> Just JsonCursorBool
  'f' -> Just JsonCursorBool
  '0' -> Just JsonCursorNumber
  '1' -> Just JsonCursorNumber
  '2' -> Just JsonCursorNumber
  '3' -> Just JsonCursorNumber
  '4' -> Just JsonCursorNumber
  '5' -> Just JsonCursorNumber
  '6' -> Just JsonCursorNumber
  '7' -> Just JsonCursorNumber
  '8' -> Just JsonCursorNumber
  '9' -> Just JsonCursorNumber
  '+' -> Just JsonCursorNumber
  '-' -> Just JsonCursorNumber
  'n' -> Just JsonCursorNull
  '{' -> Just JsonCursorObject
  '"' -> Just JsonCursorString
  _   -> Nothing

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
