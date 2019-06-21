{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Backend.Standard.Cursor.Specific
  ( SpecificCursor(..)
  , jsonCursorPos
  ) where

import HaskellWorks.Data.Json.Backend.Standard.Cursor.Generic

class SpecificCursor w where
  type CursorOf w
