module HaskellWorks.Data.Json.Internal.Doc
  ( hEncloseSep
  ) where

import Text.PrettyPrint.ANSI.Leijen

hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r s ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> hcat (zipWith (<>) (l : repeat s) ds) <> r
