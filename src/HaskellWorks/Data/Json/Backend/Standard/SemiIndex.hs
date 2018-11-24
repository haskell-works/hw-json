{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Json.Backend.Standard.SemiIndex
  ( semiIndexBuilder
  , SemiIndex(..)
  ) where

import qualified Data.ByteString.Builder                                             as B
import qualified Data.ByteString.Lazy                                                as LBS
import qualified HaskellWorks.Data.ByteString                                        as BS
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.Blank              as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.MakeIndex          as J
import qualified HaskellWorks.Data.Json.Internal.Backend.Standard.ToBalancedParens64 as J

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

data SemiIndex v = SemiIndex
  { semiIndexIb :: !v
  , semiIndexBp :: !v
  } deriving (Functor, Traversable, Foldable)

semiIndexBuilder :: LBS.ByteString -> SemiIndex B.Builder
semiIndexBuilder lbs = SemiIndex (B.lazyByteString ibs) (B.byteString (BS.toByteString bps))
  where blankedJson = J.blankJson (LBS.toChunks lbs)
        ibs = LBS.fromChunks (J.blankedJsonToInterestBits blankedJson)
        bps = J.toBalancedParens64 (J.BlankedJson blankedJson)
{-# INLINE semiIndexBuilder #-}
