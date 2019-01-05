module HaskellWorks.Data.Json.Backend.Simple.Value where

import Data.Maybe
import Data.Word
import HaskellWorks.Data.Json.Backend.Simple.Cursor
import HaskellWorks.Data.Positioning
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

snippetPos :: JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64)) -> (Count, Count)
snippetPos k = (kpa, kpz)
  where kpa   = select1 kib kta + km
        kpz   = select1 kib ktz - km
        kib   = interests k
        kbp   = balancedParens k
        kra   = cursorRank k
        krz   = fromMaybe maxBound (BP.findClose kbp kra)
        ksa   = kra + 1
        ksz   = krz + 1
        kta   = ksa `div` 2
        ktz   = ksz `div` 2
        km    = ksa `mod` 2

snippet :: JsonCursor BS.ByteString Poppy512 (BP.SimpleBalancedParens (DVS.Vector Word64)) -> BS.ByteString
snippet k = let (a, z) = snippetPos k in BS.take (fromIntegral (z - a + 1)) (BS.drop (fromIntegral (a - 1)) kt)
  where kt    = cursorText k
