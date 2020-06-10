{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Example2 where

import qualified HaskellWorks.Data.ByteString                as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF

example :: IO ()
example = do
  !jsonBs <- BS.mmap "corpus/bench/hospitalisation.json"
  let !ibip = JCF.simdToIbBp jsonBs
  let !c    = JCF.fromBsIbBp jsonBs ibip
  return ()
