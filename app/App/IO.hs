module App.IO where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import System.IO                    (Handle)

import qualified Data.ByteString.Lazy as LBS
import qualified System.IO            as IO

openOutputFile :: MonadResource m => FilePath -> Maybe Int -> m (ReleaseKey, Handle)
openOutputFile "-" _ = allocate (return IO.stdout) (const (return ()))
openOutputFile filePath maybeBufferSize = allocate open close
  where open  = do
          handle <- IO.openFile filePath IO.WriteMode
          forM_ maybeBufferSize $ \bufferSize ->
            liftIO $ IO.hSetBuffering handle (IO.BlockBuffering (Just bufferSize))
          return handle
        close = IO.hClose

readInputFile :: FilePath -> IO LBS.ByteString
readInputFile "-"      = LBS.hGetContents IO.stdin
readInputFile filePath = LBS.readFile filePath

writeOutputFile :: FilePath -> LBS.ByteString -> IO ()
writeOutputFile "-"      bs = LBS.hPut IO.stdout bs
writeOutputFile filePath bs = LBS.writeFile filePath bs
