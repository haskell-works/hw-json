{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Example4 where

import Control.Monad
import Data.Function
import Data.List
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Json.Standard.Cursor.Load.Cursor
import HaskellWorks.Data.Json.Standard.Load.Partial
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro

import qualified Data.DList as DL

{- HLINT ignore "Reduce duplication" -}

example :: IO ()
example = do
  !cursor <- loadPartial "corpus/bench/78mb.json"
  !cursor <- loadCursorWithIndex "corpus/bench/78mb.json"
  !cursor <- loadCursor "corpus/bench/78mb.json"
  !cursor <- loadCursorWithCsPoppyIndex "corpus/bench/78mb.json"
  let !json = jsonPartialJsonValueAt cursor
  let q = MQuery (DL.singleton json)

  putPretty $ q >>= item & limit 10
  putPretty $ q >>= item & page 10 1
  putPretty $ q >>= item >>= hasKV "founded_year" (JsonPartialNumber 2005) & limit 10
  putPretty $ q >>= item >>= entry
  putPretty $ q >>= item >>= entry >>= named "name" & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code")
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code") & onList (uniq . sort)
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount") & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & aggregate sum

  putPretty $ q >>= item & limit 10
  putPretty $ q >>= item & page 10 1
  putPretty $ q >>= item >>= entry
  putPretty $ q >>= item >>= entry >>= named "name" & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString)
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString) & onList (uniq . sort)
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount") & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & limit 10
  putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & aggregate sum

  return ()
