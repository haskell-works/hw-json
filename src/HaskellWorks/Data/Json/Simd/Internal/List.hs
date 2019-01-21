module HaskellWorks.Data.Json.Simd.Internal.List
  ( zipPadded
  ) where

zipPadded :: a -> b -> [a] -> [b] -> [(a, b)]
zipPadded a b (c:cs) (d:ds) = (c, d):zipPadded a b cs ds
zipPadded a b []     (d:ds) = (a, d):zipPadded a b [] ds
zipPadded a b (c:cs) []     = (c, b):zipPadded a b cs []
zipPadded _ _ []     []     = []
