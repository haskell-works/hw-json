{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Standard.Cursor.Specific
  ( SpecificCursor(..)
  , jsonCursorPos
  ) where

import HaskellWorks.Data.Json.Standard.Cursor.Generic

class SpecificCursor w where
  type CursorOf w
