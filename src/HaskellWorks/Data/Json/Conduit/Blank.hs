{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Conduit.Blank
  ( blankJson
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource         (MonadThrow)
import           Data.ByteString                      as BS
import           Data.Conduit
import           Data.Word
import           Data.Word8
import           HaskellWorks.Data.Json.Conduit.Words
import           Prelude                              as P

data BlankState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

blankJson :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankJson = blankJson' InJson

blankJson' :: MonadThrow m => BlankState -> Conduit BS.ByteString m BS.ByteString
blankJson' lastState = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (!cs, Just (!nextState, _)) = unfoldrN (BS.length bs) blankByteString (lastState, bs)
      yield cs
      blankJson' nextState
    Nothing -> return ()
  where
    blankByteString :: (BlankState, ByteString) -> Maybe (Word8, (BlankState, ByteString))
    blankByteString (InJson, bs) = case BS.uncons bs of
      Just (!c, !cs) | isLeadingDigit c   -> Just (_1          , (InNumber , cs))
      Just (!c, !cs) | c == _quotedbl     -> Just (_parenleft  , (InString , cs))
      Just (!c, !cs) | isAlphabetic c     -> Just (c           , (InIdent  , cs))
      Just (!c, !cs)                      -> Just (c           , (InJson   , cs))
      Nothing -> Nothing
    blankByteString (InString, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == _backslash    -> Just (_space      , (Escaped  , cs))
      Just (!c, !cs) | c == _quotedbl     -> Just (_parenright , (InJson   , cs))
      Just (_ , !cs)                      -> Just (_space      , (InString , cs))
      Nothing                             -> Nothing
    blankByteString (Escaped, bs) = case BS.uncons bs of
      Just (_, !cs)                       -> Just (_space, (InString, cs))
      Nothing                             -> Nothing
    blankByteString (InNumber, bs) = case BS.uncons bs of
      Just (!c, !cs) | isTrailingDigit c  -> Just (_0          , (InNumber , cs))
      Just (!c, !cs) | c == _quotedbl     -> Just (_parenleft  , (InString , cs))
      Just (!c, !cs) | isAlphabetic c     -> Just (c           , (InIdent  , cs))
      Just (!c, !cs)                      -> Just (c           , (InJson   , cs))
      Nothing                             -> Nothing
    blankByteString (InIdent, bs) = case BS.uncons bs of
      Just (!c, !cs) | isAlphabetic c     -> Just (_underscore , (InIdent  , cs))
      Just (!c, !cs) | isLeadingDigit c   -> Just (_1          , (InNumber , cs))
      Just (!c, !cs) | c == _quotedbl     -> Just (_parenleft  , (InString , cs))
      Just (!c, !cs)                      -> Just (c           , (InJson   , cs))
      Nothing                             -> Nothing
