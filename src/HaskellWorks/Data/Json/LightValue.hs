{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module HaskellWorks.Data.Json.LightValue
  ( JsonLightValue(..)
  , JsonLightValueAt(..)
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

import           Control.Arrow
import qualified Data.Attoparsec.ByteString.Char8             as ABC
import qualified Data.ByteString                              as BS
import qualified Data.DList                                   as DL
import           HaskellWorks.Data.AtLeastSize
import           HaskellWorks.Data.Entry
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.Mini
import           HaskellWorks.Data.MQuery
import           HaskellWorks.Data.Json.Succinct.PartialIndex
import           HaskellWorks.Data.Json.Value.Internal
import           HaskellWorks.Data.Row
import           Text.PrettyPrint.ANSI.Leijen

data JsonLightValue
  = JsonLightString String
  | JsonLightNumber Double
  | JsonLightObject [(String, JsonLightValue)]
  | JsonLightArray [JsonLightValue]
  | JsonLightBool Bool
  | JsonLightNull
  | JsonLightError String
  deriving (Eq, Show, Ord)

class JsonLightValueAt a where
  jsonLightJsonValueAt :: a -> JsonLightValue

jsonLightValueString :: JsonLightValue -> String
jsonLightValueString pjv = case pjv of
  JsonLightString s -> s
  _                   -> ""

instance JsonLightValueAt JsonPartialIndex where
  jsonLightJsonValueAt i = case i of
    JsonPartialIndexString s  -> case ABC.parse parseJsonString s of
      ABC.Fail    {}          -> JsonLightError ("Invalid string: '" ++ show (BS.take 20 s) ++ "...'")
      ABC.Partial _           -> JsonLightError "Unexpected end of string"
      ABC.Done    _ r         -> JsonLightString r
    JsonPartialIndexNumber s  -> case ABC.parse ABC.rational s of
      ABC.Fail    {}    -> JsonLightError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'")
      ABC.Partial f     -> case f " " of
        ABC.Fail    {}    -> JsonLightError ("Invalid number: '" ++ show (BS.take 20 s) ++ "...'")
        ABC.Partial _     -> JsonLightError "Unexpected end of number"
        ABC.Done    _ r   -> JsonLightNumber r
      ABC.Done    _ r   -> JsonLightNumber r
    JsonPartialIndexObject  fs -> JsonLightObject (map ((jsonLightValueString . parseString) *** jsonLightJsonValueAt) fs)
    JsonPartialIndexArray   es -> JsonLightArray (map jsonLightJsonValueAt es)
    JsonPartialIndexBool    v  -> JsonLightBool v
    JsonPartialIndexNull       -> JsonLightNull
    JsonPartialIndexError s    -> JsonLightError s
    where parseString bs = case ABC.parse parseJsonString bs of
            ABC.Fail    {}  -> JsonLightError ("Invalid field: '" ++ show (BS.take 20 bs) ++ "...'")
            ABC.Partial _   -> JsonLightError "Unexpected end of field"
            ABC.Done    _ s -> JsonLightString s

data JsonLightField = JsonLightField String JsonLightValue

toJsonLightField :: (String, JsonLightValue) -> JsonLightField
toJsonLightField (k, v) = JsonLightField k v

instance Pretty JsonLightField where
  pretty (JsonLightField k v) = text (show k) <> text ": " <> pretty v

hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r s ds
    = case ds of
        []  -> l <> r
        [d] -> l <> d <> r
        _   -> hcat (zipWith (<>) (l : repeat s) ds) <> r

instance Pretty JsonLightValue where
  pretty mjpv = case mjpv of
    JsonLightString s   -> dullgreen  (text (show s))
    JsonLightNumber n   -> cyan       (text (show n))
    JsonLightObject []  -> text "{}"
    JsonLightObject kvs -> hEncloseSep (text "{") (text "}") (text ",") ((pretty . toJsonLightField) `map` kvs)
    JsonLightArray vs   -> hEncloseSep (text "[") (text "]") (text ",") (pretty `map` vs)
    JsonLightBool w     -> red (text (show w))
    JsonLightNull       -> text "null"
    JsonLightError s    -> text "<error " <> text s <> text ">"

instance Pretty (Micro JsonLightValue) where
  pretty (Micro (JsonLightString s )) = dullgreen (text (show s))
  pretty (Micro (JsonLightNumber n )) = cyan      (text (show n))
  pretty (Micro (JsonLightObject [])) = text "{}"
  pretty (Micro (JsonLightObject _ )) = text "{..}"
  pretty (Micro (JsonLightArray [] )) = text "[]"
  pretty (Micro (JsonLightArray _  )) = text "[..]"
  pretty (Micro (JsonLightBool w   )) = red (text (show w))
  pretty (Micro  JsonLightNull      ) = text "null"
  pretty (Micro (JsonLightError s  )) = text "<error " <> text s <> text ">"

instance Pretty (Micro (String, JsonLightValue)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance Pretty (Mini JsonLightValue) where
  pretty mjpv = case mjpv of
    Mini (JsonLightString s   ) -> dullgreen  (text (show s))
    Mini (JsonLightNumber n   ) -> cyan       (text (show n))
    Mini (JsonLightObject []  ) -> text "{}"
    Mini (JsonLightObject kvs ) -> case kvs of
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> text "{" <> prettyKvs kvs <> text ", ..}"
      []                          -> text "{}"
      _                           -> text "{" <> prettyKvs kvs <> text "}"
    Mini (JsonLightArray []   ) -> text "[]"
    Mini (JsonLightArray vs   ) | vs `atLeastSize` 11 -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text ", ..]"
    Mini (JsonLightArray vs   ) | vs `atLeastSize` 1  -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text "]"
    Mini (JsonLightArray _    )                       -> text "[]"
    Mini (JsonLightBool w     ) -> red (text (show w))
    Mini  JsonLightNull         -> text "null"
    Mini (JsonLightError s    ) -> text "<error " <> text s <> text ">"

instance Pretty (Mini (String, JsonLightValue)) where
  pretty (Mini (fieldName, jpv)) = text (show fieldName) <> text ": " <> pretty (Mini jpv)

instance Pretty (MQuery JsonLightValue) where
  pretty = pretty . Row 120 . mQuery

instance Pretty (MQuery (Entry String JsonLightValue)) where
  pretty (MQuery das) = pretty (Row 120 das)

hasKV :: String -> JsonLightValue -> JsonLightValue -> MQuery JsonLightValue
hasKV k v (JsonLightObject xs)  = if (k, v) `elem` xs then MQuery (DL.singleton (JsonLightObject xs)) else MQuery DL.empty
hasKV _ _  _                    = MQuery DL.empty

item :: JsonLightValue -> MQuery JsonLightValue
item jpv = case jpv of
  JsonLightArray es -> MQuery $ DL.fromList es
  _                 -> MQuery   DL.empty

entry :: JsonLightValue -> MQuery (Entry String JsonLightValue)
entry jpv = case jpv of
  JsonLightObject fs  -> MQuery $ DL.fromList (uncurry Entry `map` fs)
  _                   -> MQuery   DL.empty

asString :: JsonLightValue -> MQuery String
asString jpv = case jpv of
  JsonLightString s -> MQuery $ DL.singleton s
  _                 -> MQuery   DL.empty

asInteger :: JsonLightValue -> MQuery Integer
asInteger jpv = case jpv of
  JsonLightNumber n -> MQuery $ DL.singleton (floor n)
  _                 -> MQuery   DL.empty

castAsInteger :: JsonLightValue -> MQuery Integer
castAsInteger jpv = case jpv of
  JsonLightString n -> MQuery $ DL.singleton (read n)
  JsonLightNumber n -> MQuery $ DL.singleton (floor n)
  _                 -> MQuery   DL.empty

named :: String -> Entry String JsonLightValue -> MQuery JsonLightValue
named fieldName (Entry fieldName' jpv) | fieldName == fieldName'  = MQuery $ DL.singleton jpv
named _         _                                                 = MQuery   DL.empty

jsonKeys :: JsonLightValue -> [String]
jsonKeys jpv = case jpv of
  JsonLightObject fs  -> fst `map` fs
  _                     -> []

hasKey :: String -> JsonLightValue -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

jsonSize :: JsonLightValue -> MQuery JsonLightValue
jsonSize jpv = case jpv of
  JsonLightArray  es  -> MQuery (DL.singleton (JsonLightNumber (fromIntegral (length es))))
  JsonLightObject es  -> MQuery (DL.singleton (JsonLightNumber (fromIntegral (length es))))
  _                   -> MQuery (DL.singleton (JsonLightNumber 0))
