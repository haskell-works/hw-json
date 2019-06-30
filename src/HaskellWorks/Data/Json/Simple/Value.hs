module HaskellWorks.Data.Json.Simple.Value where

import Data.Maybe
import HaskellWorks.Data.Json.Simple.Cursor
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                           as BS
import qualified HaskellWorks.Data.BalancedParens          as BP
import qualified HaskellWorks.Data.BalancedParens.RangeMin as RM

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

snippetPos :: JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy) -> (Count, Count)
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

snippet :: JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy) -> BS.ByteString
snippet k = let (a, z) = snippetPos k in BS.take (fromIntegral (z - a + 1)) (BS.drop (fromIntegral (a - 1)) kt)
  where kt    = cursorText k
