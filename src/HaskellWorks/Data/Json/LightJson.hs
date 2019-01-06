{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.LightJson where

import Control.Arrow
import Data.String
import Data.Word
import Data.Word8
import HaskellWorks.Data.Json.Internal.Doc
import HaskellWorks.Data.Json.Internal.Word8
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.AtLeastSize
import HaskellWorks.Data.MQuery.Entry
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.MQuery.Mini
import HaskellWorks.Data.MQuery.Row
import Prelude                               hiding (drop)
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List             as L

data LightJson c
  = LightJsonString String
  | LightJsonNumber BS.ByteString
  | LightJsonObject [(String, c)]
  | LightJsonArray [c]
  | LightJsonBool Bool
  | LightJsonNull
  | LightJsonError String
  deriving Show

instance Eq (LightJson c) where
  (==) (LightJsonString a) (LightJsonString b) = a == b
  (==) (LightJsonNumber a) (LightJsonNumber b) = a == b
  (==) (LightJsonBool   a) (LightJsonBool   b) = a == b
  (==)  LightJsonNull       LightJsonNull      = True
  (==)  _                   _                  = False

data LightJsonField c = LightJsonField String (LightJson c)

class LightJsonAt a where
  lightJsonAt :: a -> LightJson a

data JsonState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

slurpString :: BS.ByteString -> String
slurpString bs = L.unfoldr genString (InJson, BSC.unpack bs)
  where genString :: (JsonState, String) -> Maybe (Char, (JsonState, String))
        genString (InJson, ds) = case ds of
          (e:es) | e == '"' -> genString  (InString , es)
          (_:es)            -> genString  (InJson   , es)
          _                 -> Nothing
        genString (InString, ds) = case ds of
          (e:es) | e == '\\' -> genString  (Escaped  , es)
          (e:_ ) | e == '"'  -> Nothing
          (e:es)             -> Just (e,   (InString , es))
          _                  -> Nothing
        genString (Escaped, ds) = case ds of
          (_:es) -> Just ('.', (InString , es))
          _      -> Nothing
        genString (_, _) = Nothing

slurpNumber :: BS.ByteString -> BS.ByteString
slurpNumber bs = let (!cs, _) = BS.unfoldrN (BS.length bs) genNumber (InJson, bs) in cs
    where genNumber :: (JsonState, BS.ByteString) -> Maybe (Word8, (JsonState, BS.ByteString))
          genNumber (InJson, cs) = case BS.uncons cs of
            Just (!d, !ds) | isLeadingDigit d -> Just (d           , (InNumber , ds))
            Just (!d, !ds)                    -> Just (d           , (InJson   , ds))
            Nothing                           -> Nothing
          genNumber (InNumber, cs) = case BS.uncons cs of
            Just (!d, !ds) | isTrailingDigit d -> Just (d           , (InNumber , ds))
            Just (!d, !ds) | d == _quotedbl    -> Just (_parenleft  , (InString , ds))
            _                                  -> Nothing
          genNumber (_, _) = Nothing

toLightJsonField :: (String, LightJson c) -> LightJsonField c
toLightJsonField (k, v) = LightJsonField k v

instance LightJsonAt c => Pretty (LightJsonField c) where
  pretty (LightJsonField k v) = text (show k) <> text ": " <> pretty v

instance LightJsonAt c => Pretty (LightJson c) where
  pretty c = case c of
    LightJsonString s   -> dullgreen  (text (show s))
    LightJsonNumber n   -> cyan       (text (show n))
    LightJsonObject []  -> text "{}"
    LightJsonObject kvs -> hEncloseSep (text "{") (text "}") (text ",") ((pretty . toLightJsonField . second lightJsonAt) `map` kvs)
    LightJsonArray vs   -> hEncloseSep (text "[") (text "]") (text ",") ((pretty . lightJsonAt) `map` vs)
    LightJsonBool w     -> red (text (show w))
    LightJsonNull       -> text "null"
    LightJsonError s    -> text "<error " <> text s <> text ">"

instance Pretty (Micro (LightJson c)) where
  pretty (Micro (LightJsonString s )) = dullgreen (text (show s))
  pretty (Micro (LightJsonNumber n )) = cyan      (text (show n))
  pretty (Micro (LightJsonObject [])) = text "{}"
  pretty (Micro (LightJsonObject _ )) = text "{..}"
  pretty (Micro (LightJsonArray [] )) = text "[]"
  pretty (Micro (LightJsonArray _  )) = text "[..]"
  pretty (Micro (LightJsonBool w   )) = red (text (show w))
  pretty (Micro  LightJsonNull      ) = text "null"
  pretty (Micro (LightJsonError s  )) = text "<error " <> text s <> text ">"

instance Pretty (Micro (String, LightJson c)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance LightJsonAt c => Pretty (Mini (LightJson c)) where
  pretty mjpv = case mjpv of
    Mini (LightJsonString s   ) -> dullgreen  (text (show s))
    Mini (LightJsonNumber n   ) -> cyan       (text (show n))
    Mini (LightJsonObject []  ) -> text "{}"
    Mini (LightJsonObject kvs ) -> case kvs of
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> text "{" <> prettyKvs (map (second lightJsonAt) kvs) <> text ", ..}"
      []                          -> text "{}"
      _                           -> text "{" <> prettyKvs (map (second lightJsonAt) kvs) <> text "}"
    Mini (LightJsonArray []   ) -> text "[]"
    Mini (LightJsonArray vs   ) | vs `atLeastSize` 11 -> text "[" <> nest 2 (prettyVs ((Micro . lightJsonAt) `map` take 10 vs)) <> text ", ..]"
    Mini (LightJsonArray vs   ) | vs `atLeastSize` 1  -> text "[" <> nest 2 (prettyVs ((Micro . lightJsonAt) `map` take 10 vs)) <> text "]"
    Mini (LightJsonArray _    )                       -> text "[]"
    Mini (LightJsonBool w     ) -> red (text (show w))
    Mini  LightJsonNull         -> text "null"
    Mini (LightJsonError s    ) -> text "<error " <> text s <> text ">"

instance LightJsonAt c => Pretty (Mini (String, LightJson c)) where
  pretty (Mini (fieldName, jpv)) = text (show fieldName) <> text ": " <> pretty (Mini jpv)

instance LightJsonAt c => Pretty (MQuery (LightJson c)) where
  pretty = pretty . Row 120 . mQuery

instance LightJsonAt c => Pretty (MQuery (Entry String (LightJson c))) where
  pretty (MQuery das) = pretty (Row 120 das)
