{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString                                  as BS
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           GHC.Conc
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Diagnostics.Time
import           System.Mem

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  print "Read file"
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  print "Created cursor"
  return cursor

main :: IO ()
main = do
  performGC
  !c0 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c1 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c2 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c3 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c4 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c5 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c6 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c7 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c8 <- readJson "/Users/jky/Downloads/78mbs.json"
  !c9 <- readJson "/Users/jky/Downloads/78mbs.json"
  print "Returned from readJson"
  performGC
  threadDelay 100000000
  print c0
  print c1
  print c2
  print c3
  print c4
  print c5
  print c6
  print c7
  print c8
  print c9
