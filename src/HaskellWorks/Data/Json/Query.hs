{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Query where

import Control.Arrow
import Data.String
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Entry
import Prelude                          hiding (drop)

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.DList                       as DL

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

