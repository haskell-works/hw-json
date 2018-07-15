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
import HaskellWorks.Data.AtLeastSize
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Entry
import HaskellWorks.Data.Json.CharLike
import HaskellWorks.Data.Json.Cursor
import HaskellWorks.Data.Json.Internal.Words
import HaskellWorks.Data.Micro
import HaskellWorks.Data.Mini
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Row
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Uncons
import Prelude                                   hiding (drop)
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Data.DList                       as DL
import qualified Data.List                        as L
import qualified HaskellWorks.Data.BalancedParens as BP

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

-- instance Ord (LightJson c) where
--   compare (LightJsonString a) (LightJsonString b)  = a `compare` b
--   compare (LightJsonNumber a) (LightJsonNumber b)  = a `compare` b
--   compare (LightJsonBool   a) (LightJsonBool   b)  = a `compare` b
--   compare  LightJsonNull       LightJsonNull       = True

data (LightJsonField c) = LightJsonField String (LightJson c)

class LightJsonAt a where
  lightJsonAt :: a -> LightJson a

wSpace :: Word8
wSpace = 0x20

data JsonState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

slurpByteString :: BS.ByteString -> BS.ByteString
slurpByteString bs = let (!cs, _) = BS.unfoldrN (BS.length bs) genString (InJson, bs) in cs
  where genString :: (JsonState, BS.ByteString) -> Maybe (Word8, (JsonState, BS.ByteString))
        genString (InJson, cs) = case BS.uncons cs of
          Just (!e, !es) | e == _quotedbl     -> genString            (InString , es)
          -- TODO: Only match whitespace
          Just (!_, !es) -> genString            (InJson   , es)
          Nothing        -> Nothing
        genString (InString, ds) = case BS.uncons ds of
          Just (!e, !es) | e == _backslash    -> genString            (Escaped  , es)
          Just (!e, !_ ) | e == _quotedbl     -> Nothing
          Just (e , !es) -> Just (e            , (InString , es))
          Nothing        -> Nothing
        genString (Escaped, ds) = case BS.uncons ds of
          Just (_ , !es) -> Just (_period      , (InString , es))
          Nothing        -> Nothing
        genString (_, _) = Nothing

slurpString :: BS.ByteString -> String
slurpString bs = L.unfoldr genString (InJson, BSC.unpack bs)
  where genString :: (JsonState, String) -> Maybe (Char, (JsonState, String))
        genString (InJson, ds) = case ds of
          (e:es) | e == '"'  -> genString  (InString , es)
          (_:es) -> genString  (InJson   , es)
          _      -> Nothing
        genString (InString, ds) = case ds of
          (e:es) | e == '\\'  -> genString  (Escaped  , es)
          (e:_ ) | e == '"'   -> Nothing
          (e:es) -> Just (e,   (InString , es))
          _      -> Nothing
        genString (Escaped, ds) = case ds of
          (_:es) -> Just ('.', (InString , es))
          _      -> Nothing
        genString (_, _) = Nothing

slurpNumber :: BS.ByteString -> BS.ByteString
slurpNumber bs = let (!cs, _) = BS.unfoldrN (BS.length bs) genNumber (InJson, bs) in cs
    where genNumber :: (JsonState, BS.ByteString) -> Maybe (Word8, (JsonState, BS.ByteString))
          genNumber (InJson, cs) = case BS.uncons cs of
            Just (!d, !ds) | isLeadingDigit d   -> Just (d           , (InNumber , ds))
            Just (!d, !ds) -> Just (d           , (InJson   , ds))
            Nothing        -> Nothing
          genNumber (InNumber, cs) = case BS.uncons cs of
            Just (!d, !ds) | isTrailingDigit d  -> Just (d           , (InNumber , ds))
            Just (!d, !ds) | d == _quotedbl     -> Just (_parenleft  , (InString , ds))
            _              -> Nothing
          genNumber (_, _) = Nothing

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => LightJsonAt (JsonCursor BS.ByteString v w) where
  lightJsonAt k = case uncons remainder of
    Just (!c, _) | isLeadingDigit2 c  -> LightJsonNumber  (slurpNumber remainder)
    Just (!c, _) | isQuotDbl c        -> LightJsonString  (slurpString remainder)
    Just (!c, _) | isChar_t c         -> LightJsonBool    True
    Just (!c, _) | isChar_f c         -> LightJsonBool    False
    Just (!c, _) | isChar_n c         -> LightJsonNull
    Just (!c, _) | isBraceLeft c      -> LightJsonObject (mapValuesFrom   (firstChild k))
    Just (!c, _) | isBracketLeft c    -> LightJsonArray  (arrayValuesFrom (firstChild k))
    Just _       -> LightJsonError "Invalid Json Type"
    Nothing      -> LightJsonError "End of data"
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

toLightJsonField :: (String, LightJson c) -> LightJsonField c
toLightJsonField (k, v) = LightJsonField k v

instance LightJsonAt c => Pretty (LightJsonField c) where
  pretty (LightJsonField k v) = text (show k) <> text ": " <> pretty v

hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r s ds
    = case ds of
        []  -> l <> r
        [d] -> l <> d <> r
        _   -> hcat (zipWith (<>) (l : repeat s) ds) <> r

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

-- hasKV :: LightJsonAt c => BS.ByteString -> LightJson c -> LightJson c -> MQuery (LightJson c)
-- hasKV k v (LightJsonObject xs)  = let ys = second lightJsonAt `map` xs in
--                                   if (k, v) `elem` ys then MQuery (DL.singleton (LightJsonObject xs)) else MQuery DL.empty
-- hasKV _ _  _                    = MQuery DL.empty

item :: LightJsonAt c => LightJson c -> MQuery (LightJson c)
item jpv = case jpv of
  LightJsonArray es -> MQuery $ DL.fromList (lightJsonAt `map` es)
  _                 -> MQuery   DL.empty

entry :: LightJsonAt c => LightJson c -> MQuery (Entry String (LightJson c))
entry jpv = case jpv of
  LightJsonObject fs -> MQuery $ DL.fromList ((uncurry Entry . second lightJsonAt) `map` fs)
  _                  -> MQuery   DL.empty

asString :: LightJson c -> MQuery String
asString jpv = case jpv of
  LightJsonString s -> MQuery $ DL.singleton s
  _                 -> MQuery   DL.empty

asDouble :: LightJson c -> MQuery Double
asDouble jpv = case jpv of
  LightJsonNumber sn  -> case ABC.parse ABC.rational sn of
    ABC.Fail    {}    -> MQuery DL.empty
    ABC.Partial f     -> case f " " of
      ABC.Fail    {}  -> MQuery DL.empty
      ABC.Partial _   -> MQuery DL.empty
      ABC.Done    _ r -> MQuery (DL.singleton r)
    ABC.Done    _ r   -> MQuery (DL.singleton r)
  _                   -> MQuery   DL.empty

asInteger :: LightJson c -> MQuery Integer
asInteger jpv = do
  d <- asDouble jpv
  return (floor d)

castAsInteger :: LightJson c -> MQuery Integer
castAsInteger jpv = case jpv of
  LightJsonString n -> MQuery $ DL.singleton (read n)
  LightJsonNumber _ -> asInteger jpv
  _                 -> MQuery   DL.empty

named :: String -> Entry String (LightJson c) -> MQuery (LightJson c)
named fieldName (Entry fieldName' jpv) | fieldName == fieldName'  = MQuery $ DL.singleton jpv
named _         _                      = MQuery   DL.empty

jsonKeys :: LightJson c -> [String]
jsonKeys jpv = case jpv of
  LightJsonObject fs -> fst `map` fs
  _                  -> []

hasKey :: String -> LightJson c -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

jsonSize :: LightJson c -> MQuery Integer
jsonSize jpv = case jpv of
  LightJsonArray  es -> MQuery (DL.singleton (fromIntegral (length es)))
  LightJsonObject es -> MQuery (DL.singleton (fromIntegral (length es)))
  _                  -> MQuery (DL.singleton 0)
