{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.PartialValue
  ( JsonPartialValue(..)
  , JsonPartialValueAt(..)
  , asInteger
  , asString
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
import HaskellWorks.Data.AtLeastSize
import HaskellWorks.Data.Entry
import HaskellWorks.Data.Json.Internal.PartialIndex
import HaskellWorks.Data.Json.Internal.Value
import HaskellWorks.Data.Micro
import HaskellWorks.Data.Mini
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.Row
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as BS
import qualified Data.DList                       as DL

data JsonPartialValue
  = JsonPartialString String
  | JsonPartialNumber Double
  | JsonPartialObject [(String, JsonPartialValue)]
  | JsonPartialArray [JsonPartialValue]
  | JsonPartialBool Bool
  | JsonPartialNull
  | JsonPartialError String
  deriving (Eq, Show, Ord)

class JsonPartialValueAt a where
  jsonPartialJsonValueAt :: a -> JsonPartialValue

data JsonPartialField = JsonPartialField String JsonPartialValue

jsonPartialValueString :: JsonPartialValue -> String
jsonPartialValueString pjv = case pjv of
  JsonPartialString s -> s
  _                   -> ""

instance JsonPartialValueAt JsonPartialIndex where
  jsonPartialJsonValueAt i = case i of
    JsonPartialIndexString s  -> case ABC.parse parseJsonString s of
      ABC.Fail    {}  -> JsonPartialError ("Invalid string: '" ++ show (BS.take 20 s) ++ "...'")
      ABC.Partial _   -> JsonPartialError "Unexpected end of string"
      ABC.Done    _ r -> JsonPartialString r
    JsonPartialIndexNumber s  -> case ABC.parse ABC.rational s of
      ABC.Fail    {}    -> JsonPartialError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'")
      ABC.Partial f     -> case f " " of
        ABC.Fail    {}  -> JsonPartialError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'")
        ABC.Partial _   -> JsonPartialError "Unexpected end of number"
        ABC.Done    _ r -> JsonPartialNumber r
      ABC.Done    _ r   -> JsonPartialNumber r
    JsonPartialIndexObject  fs -> JsonPartialObject (map ((jsonPartialValueString . parseString) *** jsonPartialJsonValueAt) fs)
    JsonPartialIndexArray   es -> JsonPartialArray (map jsonPartialJsonValueAt es)
    JsonPartialIndexBool    v  -> JsonPartialBool v
    JsonPartialIndexNull       -> JsonPartialNull
    JsonPartialIndexError s    -> JsonPartialError s
    where parseString bs = case ABC.parse parseJsonString bs of
            ABC.Fail    {}  -> JsonPartialError ("Invalid field: '" ++ show (BS.take 20 bs) ++ "...'")
            ABC.Partial _   -> JsonPartialError "Unexpected end of field"
            ABC.Done    _ s -> JsonPartialString s

toJsonPartialField :: (String, JsonPartialValue) -> JsonPartialField
toJsonPartialField (k, v) = JsonPartialField k v

instance Pretty JsonPartialField where
  pretty (JsonPartialField k v) = text (show k) <> text ": " <> pretty v

hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r s ds
    = case ds of
        []  -> l <> r
        [d] -> l <> d <> r
        _   -> hcat (zipWith (<>) (l : repeat s) ds) <> r

instance Pretty JsonPartialValue where
  pretty mjpv = case mjpv of
    JsonPartialString s   -> dullgreen  (text (show s))
    JsonPartialNumber n   -> cyan       (text (show n))
    JsonPartialObject []  -> text "{}"
    JsonPartialObject kvs -> hEncloseSep (text "{") (text "}") (text ",") ((pretty . toJsonPartialField) `map` kvs)
    JsonPartialArray vs   -> hEncloseSep (text "[") (text "]") (text ",") (pretty `map` vs)
    JsonPartialBool w     -> red (text (show w))
    JsonPartialNull       -> text "null"
    JsonPartialError s    -> text "<error " <> text s <> text ">"

instance Pretty (Micro JsonPartialValue) where
  pretty (Micro (JsonPartialString s )) = dullgreen (text (show s))
  pretty (Micro (JsonPartialNumber n )) = cyan      (text (show n))
  pretty (Micro (JsonPartialObject [])) = text "{}"
  pretty (Micro (JsonPartialObject _ )) = text "{..}"
  pretty (Micro (JsonPartialArray [] )) = text "[]"
  pretty (Micro (JsonPartialArray _  )) = text "[..]"
  pretty (Micro (JsonPartialBool w   )) = red (text (show w))
  pretty (Micro  JsonPartialNull      ) = text "null"
  pretty (Micro (JsonPartialError s  )) = text "<error " <> text s <> text ">"

instance Pretty (Micro (String, JsonPartialValue)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance Pretty (Mini JsonPartialValue) where
  pretty mjpv = case mjpv of
    Mini (JsonPartialString s   ) -> dullgreen  (text (show s))
    Mini (JsonPartialNumber n   ) -> cyan       (text (show n))
    Mini (JsonPartialObject []  ) -> text "{}"
    Mini (JsonPartialObject kvs ) -> case kvs of
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> text "{" <> prettyKvs kvs <> text ", ..}"
      []                          -> text "{}"
      _                           -> text "{" <> prettyKvs kvs <> text "}"
    Mini (JsonPartialArray []   ) -> text "[]"
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 11 -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text ", ..]"
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 1  -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text "]"
    Mini (JsonPartialArray _    )                       -> text "[]"
    Mini (JsonPartialBool w     ) -> red (text (show w))
    Mini  JsonPartialNull         -> text "null"
    Mini (JsonPartialError s    ) -> text "<error " <> text s <> text ">"

instance Pretty (Mini (String, JsonPartialValue)) where
  pretty (Mini (fieldName, jpv)) = text (show fieldName) <> text ": " <> pretty (Mini jpv)

instance Pretty (MQuery JsonPartialValue) where
  pretty = pretty . Row 120 . mQuery

instance Pretty (MQuery (Entry String JsonPartialValue)) where
  pretty (MQuery das) = pretty (Row 120 das)

hasKV :: String -> JsonPartialValue -> JsonPartialValue -> MQuery JsonPartialValue
hasKV k v (JsonPartialObject xs) = if (k, v) `elem` xs then MQuery (DL.singleton (JsonPartialObject xs)) else MQuery DL.empty
hasKV _ _  _                     = MQuery DL.empty

item :: JsonPartialValue -> MQuery JsonPartialValue
item jpv = case jpv of
  JsonPartialArray es -> MQuery $ DL.fromList es
  _                   -> MQuery   DL.empty

entry :: JsonPartialValue -> MQuery (Entry String JsonPartialValue)
entry jpv = case jpv of
  JsonPartialObject fs -> MQuery $ DL.fromList (uncurry Entry `map` fs)
  _                    -> MQuery   DL.empty

asString :: JsonPartialValue -> MQuery String
asString jpv = case jpv of
  JsonPartialString s -> MQuery $ DL.singleton s
  _                   -> MQuery   DL.empty

asInteger :: JsonPartialValue -> MQuery Integer
asInteger jpv = case jpv of
  JsonPartialNumber n -> MQuery $ DL.singleton (floor n)
  _                   -> MQuery   DL.empty

castAsInteger :: JsonPartialValue -> MQuery Integer
castAsInteger jpv = case jpv of
  JsonPartialString n -> MQuery $ DL.singleton (read n)
  JsonPartialNumber n -> MQuery $ DL.singleton (floor n)
  _                   -> MQuery   DL.empty

named :: String -> Entry String JsonPartialValue -> MQuery JsonPartialValue
named fieldName (Entry fieldName' jpv) | fieldName == fieldName'  = MQuery $ DL.singleton jpv
named _         _                      = MQuery   DL.empty

jsonKeys :: JsonPartialValue -> [String]
jsonKeys jpv = case jpv of
  JsonPartialObject fs -> fst `map` fs
  _                    -> []

hasKey :: String -> JsonPartialValue -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

jsonSize :: JsonPartialValue -> MQuery JsonPartialValue
jsonSize jpv = case jpv of
  JsonPartialArray  es -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  JsonPartialObject es -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  _                    -> MQuery (DL.singleton (JsonPartialNumber 0))
