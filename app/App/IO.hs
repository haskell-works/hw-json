module App.IO where

import qualified Data.ByteString.Lazy as LBS
import qualified System.IO            as IO

readInputFile :: FilePath -> IO LBS.ByteString
readInputFile "-"      = LBS.hGetContents IO.stdin
readInputFile filePath = LBS.readFile filePath

writeOutputFile :: FilePath -> LBS.ByteString -> IO ()
writeOutputFile "-"      bs = LBS.hPut IO.stdout bs
writeOutputFile filePath bs = LBS.writeFile filePath bs
