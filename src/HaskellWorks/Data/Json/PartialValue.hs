{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.PartialValue
  ( JsonPartialValue(..)
  , JsonPartialValueAt(..)
  , asInteger
  , asString
  , asText
  , castAsInteger
  , entry
  , hasKey
  , hasKV
  , item
  , jsonKeys
  , jsonSize
  , named
  ) where

import Control.Arrow
import Data.String
import Data.Text                                      (Text)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Json.Internal.Doc
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.Internal.Value
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.AtLeastSize
import HaskellWorks.Data.MQuery.Entry
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.MQuery.Mini
import HaskellWorks.Data.MQuery.Row
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import Prelude                                        hiding (drop)
import Prettyprinter

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as BS
import qualified Data.DList                       as DL
import qualified Data.Text                        as T
import qualified HaskellWorks.Data.BalancedParens as BP

-- | Partial JSON type.
--
-- This data type has an additional 'JsonPartialError' data constructor to indicate parsing
-- errors.  This allows parsing to be more lazy because parsing errors may now be expressed
-- anywhere in the parsed document so the parser no longer needs to make a verdict about whether
-- there are any parsing errors in the entire document.
--
-- See 'jsonPartialJsonValueAt' on how to parse JSON text into this datatype.
--
-- Although this data type allows for lazier parsing it doesn't allow for sub-trees to be
-- garbage collected if a reference to an ancestor node is held.  To avoid holding onto
-- sub-trees that are no longer needed without having to drop references to ancestors use
-- 'HaskellWorks.Data.Json.LightJson.LightJson' instead.
data JsonPartialValue
  = JsonPartialString Text
  | JsonPartialNumber Double
  | JsonPartialObject [(Text, JsonPartialValue)]
  | JsonPartialArray [JsonPartialValue]
  | JsonPartialBool Bool
  | JsonPartialNull
  | JsonPartialError Text
  deriving (Eq, Show, Ord)

class JsonPartialValueAt a where
  -- | Get a JSON partial value from another type
  --
  -- This function can always "succeed" because the data type it returns allows for the document value
  -- to contain an arbitrary number of errors.  This means errors can be reported in document nodes
  -- as parsing occurs lazily.
  --
  -- There are garbage collection implementations you may want to consider.  If you would like to be
  -- able to hold onto ancestor nodes and still be able to garbage collect visited sub-trees, then
  -- consider using 'HaskellWorks.Data.Json.LightJson.lightJsonAt' instead.
  jsonPartialJsonValueAt :: a -> JsonPartialValue

data JsonPartialField = JsonPartialField Text JsonPartialValue

jsonPartialValueString :: JsonPartialValue -> Text
jsonPartialValueString pjv = case pjv of
  JsonPartialString s -> s
  _                   -> ""

instance JsonPartialValueAt JsonPartialIndex where
  jsonPartialJsonValueAt i = case i of
    JsonPartialIndexString s  -> case ABC.parse parseJsonString s of
      ABC.Fail    {}  -> JsonPartialError ("Invalid string: '" <> T.pack (show (BS.take 20 s)) <> "...'")
      ABC.Partial _   -> JsonPartialError "Unexpected end of string"
      ABC.Done    _ r -> JsonPartialString (T.pack r) -- TODO optimise
    JsonPartialIndexNumber s  -> case ABC.parse ABC.rational s of
      ABC.Fail    {}    -> JsonPartialError ("Invalid number: '" <> T.pack (show (BS.take 20 s)) <> "...'")
      ABC.Partial f     -> case f " " of
        ABC.Fail    {}  -> JsonPartialError ("Invalid number: '" <> T.pack (show (BS.take 20 s)) <> "...'")
        ABC.Partial _   -> JsonPartialError "Unexpected end of number"
        ABC.Done    _ r -> JsonPartialNumber r
      ABC.Done    _ r   -> JsonPartialNumber r
    JsonPartialIndexObject  fs -> JsonPartialObject (map ((jsonPartialValueString . parseString) *** jsonPartialJsonValueAt) fs)
    JsonPartialIndexArray   es -> JsonPartialArray (map jsonPartialJsonValueAt es)
    JsonPartialIndexBool    v  -> JsonPartialBool v
    JsonPartialIndexNull       -> JsonPartialNull
    JsonPartialIndexError s    -> JsonPartialError (T.pack s) -- TODO optimise
    where parseString bs = case ABC.parse parseJsonString bs of
            ABC.Fail    {}  -> JsonPartialError ("Invalid field: '" <> T.pack (show (BS.take 20 bs)) <> "...'")
            ABC.Partial _   -> JsonPartialError "Unexpected end of field"
            ABC.Done    _ s -> JsonPartialString (T.pack s) -- TODO optimise

toJsonPartialField :: (Text, JsonPartialValue) -> JsonPartialField
toJsonPartialField (k, v) = JsonPartialField k v

instance Pretty JsonPartialField where
  pretty (JsonPartialField k v) = text (show k) <> text ": " <> pretty v

instance Pretty JsonPartialValue where
  pretty mjpv = case mjpv of
    JsonPartialString s   -> dullgreen  (text (show s))
    JsonPartialNumber n   -> cyan       (text (show n))
    JsonPartialObject []  -> text "{}"
    JsonPartialObject kvs -> hEncloseSep (text "{") (text "}") (text ",") ((pretty . toJsonPartialField) `map` kvs)
    JsonPartialArray vs   -> hEncloseSep (text "[") (text "]") (text ",") (pretty `map` vs)
    JsonPartialBool w     -> red (text (show w))
    JsonPartialNull       -> text "null"
    JsonPartialError s    -> text "<error " <> text (T.unpack s) <> text ">"

instance Pretty (Micro JsonPartialValue) where
  pretty (Micro (JsonPartialString s )) = dullgreen (text (show s))
  pretty (Micro (JsonPartialNumber n )) = cyan      (text (show n))
  pretty (Micro (JsonPartialObject [])) = text "{}"
  pretty (Micro (JsonPartialObject _ )) = text "{..}"
  pretty (Micro (JsonPartialArray [] )) = text "[]"
  pretty (Micro (JsonPartialArray _  )) = text "[..]"
  pretty (Micro (JsonPartialBool w   )) = red (text (show w))
  pretty (Micro  JsonPartialNull      ) = text "null"
  pretty (Micro (JsonPartialError s  )) = text "<error " <> text (T.unpack s) <> text ">"

instance Pretty (Micro (String, JsonPartialValue)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance Pretty (Micro (Text, JsonPartialValue)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance Pretty (Mini JsonPartialValue) where
  pretty mjpv = case mjpv of
    Mini (JsonPartialString s   ) -> dullgreen  (text (show s))
    Mini (JsonPartialNumber n   ) -> cyan       (text (show n))
    Mini (JsonPartialObject kvs ) -> case kvs of
      []                          -> text "{}"
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> text "{" <> prettyKvs kvs <> text ", ..}"
      _                           -> text "{" <> prettyKvs kvs <> text "}"
    Mini (JsonPartialArray []   ) -> text "[]"
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 11 -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text ", ..]"
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 1  -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text "]"
    Mini (JsonPartialArray _    )                       -> text "[]"
    Mini (JsonPartialBool w     ) -> red (text (show w))
    Mini  JsonPartialNull         -> text "null"
    Mini (JsonPartialError s    ) -> text "<error " <> text (T.unpack s) <> text ">"

instance Pretty (Mini (Text, JsonPartialValue)) where
  pretty (Mini (fieldName, jpv)) = text (show fieldName) <> text ": " <> pretty (Mini jpv)

instance Pretty (MQuery JsonPartialValue) where
  pretty = pretty . Row 120 . mQuery

instance Pretty (MQuery (Entry Text JsonPartialValue)) where
  pretty (MQuery das) = pretty (Row 120 das)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => JsonPartialValueAt (GenericCursor BS.ByteString v w) where
  jsonPartialJsonValueAt = jsonPartialJsonValueAt . jsonPartialIndexAt

hasKV :: Text -> JsonPartialValue -> JsonPartialValue -> MQuery JsonPartialValue
hasKV k v (JsonPartialObject xs) = if (k, v) `elem` xs then MQuery (DL.singleton (JsonPartialObject xs)) else MQuery DL.empty
hasKV _ _  _                     = MQuery DL.empty

item :: JsonPartialValue -> MQuery JsonPartialValue
item jpv = case jpv of
  JsonPartialArray es -> MQuery $ DL.fromList es
  _                   -> MQuery   DL.empty

entry :: JsonPartialValue -> MQuery (Entry Text JsonPartialValue)
entry jpv = case jpv of
  JsonPartialObject fs -> MQuery $ DL.fromList (uncurry Entry `map` fs)
  _                    -> MQuery   DL.empty

asString :: JsonPartialValue -> MQuery String
asString jpv = case jpv of
  JsonPartialString s -> MQuery $ DL.singleton (T.unpack s)
  _                   -> MQuery   DL.empty

asText :: JsonPartialValue -> MQuery Text
asText jpv = case jpv of
  JsonPartialString s -> MQuery $ DL.singleton s
  _                   -> MQuery   DL.empty

asInteger :: JsonPartialValue -> MQuery Integer
asInteger jpv = case jpv of
  JsonPartialNumber n -> MQuery $ DL.singleton (floor n)
  _                   -> MQuery   DL.empty

castAsInteger :: JsonPartialValue -> MQuery Integer
castAsInteger jpv = case jpv of
  JsonPartialString n -> MQuery $ DL.singleton (read (T.unpack n))
  JsonPartialNumber n -> MQuery $ DL.singleton (floor n)
  _                   -> MQuery   DL.empty

named :: Text -> Entry Text JsonPartialValue -> MQuery JsonPartialValue
named fieldName (Entry fieldName' jpv) | fieldName == fieldName'  = MQuery $ DL.singleton jpv
named _         _                      = MQuery   DL.empty

jsonKeys :: JsonPartialValue -> [Text]
jsonKeys jpv = case jpv of
  JsonPartialObject fs -> fst `map` fs
  _                    -> []

hasKey :: Text -> JsonPartialValue -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

jsonSize :: JsonPartialValue -> MQuery JsonPartialValue
jsonSize jpv = case jpv of
  JsonPartialArray  es -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  JsonPartialObject es -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  _                    -> MQuery (DL.singleton (JsonPartialNumber 0))
