{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.LightJson where

import Control.Arrow
import Control.Monad
import Data.String
import Data.Text                                      (Text)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Json.Internal.CharLike
import HaskellWorks.Data.Json.Internal.Doc
import HaskellWorks.Data.Json.Internal.Slurp
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.AtLeastSize
import HaskellWorks.Data.MQuery.Entry
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.MQuery.Mini
import HaskellWorks.Data.MQuery.Row
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Uncons
import Prelude                                        hiding (drop)
import Prettyprinter

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Unsafe               as BSU
import qualified Data.List                            as L
import qualified Data.Text                            as T
import qualified HaskellWorks.Data.BalancedParens     as BP
import qualified HaskellWorks.Data.Json.Simple.Cursor as JSC

data LightJson c
  = LightJsonString Text
  | LightJsonNumber BS.ByteString
  | LightJsonObject [(Text, c)]
  | LightJsonArray [c]
  | LightJsonBool Bool
  | LightJsonNull
  | LightJsonError Text
  deriving Show

instance LightJsonAt c => Eq (LightJson c) where
  (==) (LightJsonString a) (LightJsonString b) = a == b
  (==) (LightJsonNumber a) (LightJsonNumber b) = a == b
  (==) (LightJsonBool   a) (LightJsonBool   b) = a == b
  (==) (LightJsonObject a) (LightJsonObject b) = fmap (fmap lightJsonAt) a == fmap (fmap lightJsonAt) b
  (==) (LightJsonArray  a) (LightJsonArray  b) = fmap lightJsonAt a == fmap lightJsonAt b
  (==)  LightJsonNull       LightJsonNull      = True
  (==) (LightJsonError  a) (LightJsonError  b) = a == b
  (==)  _                   _                  = False

data LightJsonField c = LightJsonField Text (LightJson c)

class LightJsonAt a where
  lightJsonAt :: a -> LightJson a

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
    LightJsonError s    -> text "<error " <> text (T.unpack s) <> text ">"
    where toLightJsonField :: (Text, LightJson c) -> LightJsonField c
          toLightJsonField (k, v) = LightJsonField k v

instance Pretty (Micro (LightJson c)) where
  pretty (Micro (LightJsonString s )) = dullgreen (text (show s))
  pretty (Micro (LightJsonNumber n )) = cyan      (text (show n))
  pretty (Micro (LightJsonObject [])) = text "{}"
  pretty (Micro (LightJsonObject _ )) = text "{..}"
  pretty (Micro (LightJsonArray [] )) = text "[]"
  pretty (Micro (LightJsonArray _  )) = text "[..]"
  pretty (Micro (LightJsonBool w   )) = red (text (show w))
  pretty (Micro  LightJsonNull      ) = text "null"
  pretty (Micro (LightJsonError s  )) = text "<error " <> text (T.unpack s) <> text ">"

instance Pretty (Micro (String, LightJson c)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance Pretty (Micro (Text, LightJson c)) where
  pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance LightJsonAt c => Pretty (Mini (LightJson c)) where
  pretty mjpv = case mjpv of
    Mini (LightJsonString s   ) -> dullgreen  (text (show s))
    Mini (LightJsonNumber n   ) -> cyan       (text (show n))
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
    Mini (LightJsonError s    ) -> text "<error " <> text (T.unpack s) <> text ">"

instance LightJsonAt c => Pretty (Mini (String, LightJson c)) where
  pretty (Mini (fieldName, jpv)) = text (show fieldName) <> text ": " <> pretty (Mini jpv)

instance LightJsonAt c => Pretty (Mini (Text, LightJson c)) where
  pretty (Mini (fieldName, jpv)) = text (show fieldName) <> text ": " <> pretty (Mini jpv)

instance LightJsonAt c => Pretty (MQuery (LightJson c)) where
  pretty = pretty . Row 120 . mQuery

instance LightJsonAt c => Pretty (MQuery (Entry String (LightJson c))) where
  pretty (MQuery das) = pretty (Row 120 das)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => LightJsonAt (GenericCursor BS.ByteString v w) where
  lightJsonAt k = case uncons remainder of
    Just (!c, _) | isLeadingDigit2 c -> LightJsonNumber  (slurpNumber remainder)
    Just (!c, _) | isQuotDbl c       -> either LightJsonError LightJsonString (slurpText remainder)
    Just (!c, _) | isChar_t c        -> LightJsonBool    True
    Just (!c, _) | isChar_f c        -> LightJsonBool    False
    Just (!c, _) | isChar_n c        -> LightJsonNull
    Just (!c, _) | isBraceLeft c     -> LightJsonObject (mapValuesFrom   (firstChild k))
    Just (!c, _) | isBracketLeft c   -> LightJsonArray  (arrayValuesFrom (firstChild k))
    Just _                           -> LightJsonError "Invalid Json Type"
    Nothing                          -> LightJsonError "End of data"
    where ik                = interests k
          bpk               = balancedParens k
          p                 = lastPositionOf (select1 ik (rank1 bpk (cursorRank k)))
          remainder         = drop (toCount p) (cursorText k)
          arrayValuesFrom   = L.unfoldr (fmap (id &&& nextSibling))
          mapValuesFrom j   = pairwise (arrayValuesFrom j) >>= asField
          pairwise (a:b:rs) = (a, b) : pairwise rs
          pairwise _        = []
          asField (a, b)    = case lightJsonAt a of
                                LightJsonString s -> [(s, b)]
                                _                 -> []

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => LightJsonAt (JSC.JsonCursor BS.ByteString v w) where
  lightJsonAt k = if kra `mod` 2 == 1
    then let i = fromIntegral (kpa - 1) :: Int in
      if i < BS.length kt
        then case BSU.unsafeIndex kt i of
          91  -> LightJsonArray  []
          123 -> LightJsonObject []
          _   -> LightJsonError "Invalid collection character"
        else LightJsonError "Index out of bounds"
    else LightJsonError "Unaligned cursor"
    where kpa   = select1 kib kta + km
          kib   = JSC.interests k
          kra   = JSC.cursorRank k
          ksa   = kra + 1
          kta   = ksa `div` 2
          km    = ksa `mod` 2
          kt    = JSC.cursorText k
