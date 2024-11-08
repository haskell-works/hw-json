module HaskellWorks.Data.Json.Internal.Doc
  ( hEncloseSep,
    text,
    red,
    dullgreen,
    cyan,
  ) where

import Prettyprinter

hEncloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
hEncloseSep l r s ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> hcat (zipWith (<>) (l : repeat s) ds) <> r

text :: String -> Doc a
text = pretty

red :: Doc a -> Doc a
red = id

dullgreen :: Doc a -> Doc a
dullgreen = id

cyan :: Doc a -> Doc a
cyan = id
