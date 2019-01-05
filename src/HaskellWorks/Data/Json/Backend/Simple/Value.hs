module HaskellWorks.Data.Json.Backend.Simple.Value where

import Data.Maybe
import Data.Word
import HaskellWorks.Data.Json.Backend.Simple.Cursor
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.Poppy512

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.BalancedParens as BP

data JsonValue
  = JsonValues JsonValues
  | JsonNull
  | JsonNumber Double
  | JsonString String
  | JsonBoolean Bool
  deriving (Eq, Show)

data JsonValues
  = JsonArray  [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq, Show)

snippet :: JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64)) -> BS.ByteString
snippet k = BS.take (fromIntegral (kpz - kpa)) (BS.drop (fromIntegral (kpa - 1)) kt)
  where kpa = select1 kib (kra `div` 2 + 1)
        kpz = select1 kib (krz `div` 2 + 1)
        kt  = cursorText k
        kib = interests k
        kbp = balancedParens k
        kra = cursorRank k
        krz = fromMaybe maxBound (BP.findClose kbp kra)
