{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.Function
import Data.Word
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Json.LightJson
import HaskellWorks.Data.Json.LoadCursor
import HaskellWorks.Data.Json.Succinct.Cursor
import HaskellWorks.Data.Micro
import HaskellWorks.Data.MQuery
import HaskellWorks.Diagnostics.Time

import qualified Data.ByteString      as BS
import qualified Data.DList           as DL
import qualified Data.Vector.Storable as DVS

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  print ("Read file" :: String)
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  print ("Created cursor" :: String)
  return cursor

-- exploreSiblings :: BalancedParens b => b -> Count -> ([Count] -> [Count])
-- exploreSiblings b c = case nextSibling b c of
--   Just d -> (d:) . explore b d
--   Nothing -> id
--
-- exploreChildren :: BalancedParens b => b -> Count -> ([Count] -> [Count])
-- exploreChildren b c = case firstChild b c of
--   Just d -> (d:) . explore b d
--   Nothing -> id
--
-- explore :: BalancedParens b => b -> Count -> ([Count] -> [Count])
-- explore b c = exploreSiblings b c . exploreChildren b c

main :: IO ()
main = do
  !cursor <- loadJsonWithCsPoppyIndex "../data/78mb.json"
  -- let ranks = explore (balancedParens cursor) (cursorRank cursor)
  -- putStrLn $ "Ranks: " ++ show (length (ranks []))

  let !json = lightJsonAt cursor
  let q = MQuery (DL.singleton json)

  measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString) & count
