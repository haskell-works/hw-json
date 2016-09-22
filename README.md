# hw-json
[![0.0-branch](https://circleci.com/gh/haskell-works/hw-json/tree/0.0-branch.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-json/tree/0.0-branch)

Conduits for tokenizing streams.

`hw-json` is a succinct JSON parsing library.  It uses succinct data-structures to allow traversal of
large JSON strings with minimal memory overhead.

It is currently considered experimental.

For an example, see [`app/Main.hs`](../master/app/Main.hs)

## Prerequisites
* Install `haskell-stack`.
* Install `hlint` (eg. `stack install hlint`)

## Building

Run the following in the shell:

    git clone git@github.com:haskell-works/hw-json.git
    cd hw-json
    stack setup
    stack build
    stack test
    stack ghci --ghc-options -XOverloadedStrings \
      --main-is hw-json:exe:hw-json-example

## Memory benchmark

### Parsing large Json files in Scala with Argonaut

          S0U       EU           OU       MU     CCSU CMD
    --------- --------- ----------- -------- -------- ---------------------------------------------------------------
          0.0  80,526.3    76,163.6 72,338.6 13,058.6 sbt console
          0.0 536,660.4    76,163.6 72,338.6 13,058.6 import java.io._, argonaut._, Argonaut._
          0.0 552,389.1    76,163.6 72,338.6 13,058.6 val file = new File("/Users/jky/Downloads/78mbs.json"
          0.0 634,066.5    76,163.6 72,338.6 13,058.6 val array = new Array[Byte](file.length.asInstanceOf[Int])
          0.0 644,552.3    76,163.6 72,338.6 13,058.6 val is = new FileInputStream("/Users/jky/Downloads/78mbs.json")
          0.0 655,038.1    76,163.6 72,338.6 13,058.6 is.read(array)
    294,976.0 160,159.7 1,100,365.0 79,310.8 13,748.1 val json = new String(array)
    285,182.9 146,392.6 1,956,264.5 82,679.8 14,099.6 val data = Parse.parse(json)
                        ***********

### Parsing large Json files in Haskell with Aeson

    Mem (MB) CMD
    -------- ---------------------------------------------------------
         302 import Data.Aeson
         302 import qualified  Data.ByteString.Lazy as BSL
         302 json78m <- BSL.readFile "/Users/jky/Downloads/78mbs.json"
        1400 let !x = decode json78m :: Maybe Value

### Parsing large Json files in Haskell with hw-json

    Mem (MB) CMD
    -------- ---------------------------------------------------------
         274 import Foreign
         274 import qualified Data.Vector.Storable as DVS
         274 import qualified Data.ByteString as BS
         274 import System.IO.MMap
         274 import Control.Monad
         274 import Data.Word
         274 import HaskellWorks.Data.Bits.BitShown
         274 import HaskellWorks.Data.Succinct.BalancedParens.Simple
         274 import HaskellWorks.Data.FromForeignRegion
         274 import HaskellWorks.Diagnostics
         274 (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/78mbs.json" ReadOnly Nothing
         601 cursor <- measure (fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))

## Examples

### Performance example

    import Foreign
    import qualified Data.Vector.Storable as DVS
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Internal as BSI
    import System.IO.MMap
    import Data.Word
    import System.CPUTime
    (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/78mbs.json" ReadOnly Nothing
    cursor <- measure (fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
    let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
    x <- measure $ jsonBsToInterestBs bs
    let !y = runListConduit [bs] (unescape' "")

    import Foreign
    import qualified Data.Vector.Storable as DVS
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Internal as BSI
    import System.IO.MMap
    import Data.Word
    import System.CPUTime
    (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/part40.json" ReadOnly Nothing
    let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
    x <- measure $ BS.concat $ runListConduit [bs] (blankJson =$= blankedJsonToInterestBits)
    x <- measure $ jsonBsToInterestBs bs

    jsonTokenAt $ J.nextSibling $ J.firstChild $ J.nextSibling $ J.firstChild $ J.firstChild  cursor

### Navigation example

```
$  cabal repl --ghc-option='-package mmap'
λ> :set -XNoMonomorphismRestriction
λ> import qualified Data.ByteString                                            as BS
λ> import           Data.String
λ> import qualified Data.Vector.Storable                                       as DVS
λ> import           Data.Word
λ> import           HaskellWorks.Data.Bits.BitShow
λ> import           HaskellWorks.Data.Bits.BitShown
λ> import           HaskellWorks.Data.FromForeignRegion
λ> import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
λ> import           HaskellWorks.Data.Json.Token
λ> import           HaskellWorks.Data.Succinct.BalancedParens.Internal
λ> import           HaskellWorks.Data.Succinct.BalancedParens.Simple
λ> import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
λ> import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
λ> import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
λ> import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
λ> import qualified HaskellWorks.Data.TreeCursor as TC
λ> import           System.IO.MMap
λ> let fc = TC.firstChild
λ> let ns = TC.nextSibling
λ> let pn = TC.parent
λ> let cd = TC.depth
λ> let ss = TC.subtreeSize
λ> let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
λ> cursor
```

### Querying example

```
import           Control.Monad
import qualified Data.DList as DL
import           Data.Function
import           Data.List
import           HaskellWorks.Data.Json.Load
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.MQuery
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Row
import           HaskellWorks.Diagnostics
import           Text.PrettyPrint.ANSI.Leijen
```

```
!json <- loadJsonPartial "data/78mb.json"
!json <- loadJsonWithIndex "data/78mb.json"
!json <- loadJson "data/78mb.json"
let q = MQuery (DL.singleton json)
```

```
putPretty $ q >>= item & limit 10
putPretty $ q >>= item & page 10 1
putPretty $ q >>= item >>= hasKV "founded_year" (JsonPartialNumber 2005) & limit 10
putPretty $ q >>= item >>= entry
putPretty $ q >>= item >>= entry >>= named "name" & limit 10
putPretty $ q >>= item >>= entry >>= satisfying (\(k, _) -> k == "name") >>= value & limit 10
putPretty $ q >>= item >>= entry >>= satisfying ((== "name") . fst) >>= value & limit 10
putPretty $ q >>= (item >=> entry >=> key) & limit 10
putPretty $ q >>= item >>= entry >>= key & limit 100 & onList (uniq . sort)
putPretty $ (q >>= item >>= entry & limit 1) >>= field "name" & limit 10
putPretty $ do {j <- q; e <- item j; (k, v) <- entry e; return k}
putPretty $ do {j <- q; e <- item j; (k, v) <- entry e; guard (k == "name"); return v}
```

### Decoding
#### Line separated base 64 encoded gzipped json
while read in; do echo "$in" | base64 --decode | gunzip; echo ""; done < file.lgz > firehose.json

### Profiling with stack traces
```
mafia build -p
cabal repl --ghc-options='-fexternal-interpreter -prof'
```

```
import HaskellWorks.Data.Succinct.BalancedParens
import HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import HaskellWorks.Data.Positioning
import qualified Data.Vector.Storable as DVS
import HaskellWorks.Data.IndexedSeq
(jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex "firehose.json"
let bp1 = SimpleBalancedParens jsonBp
let bp2 = SimpleBalancedParens (makePoppy512 jsonBp)
let bp3 = makePoppy512 jsonBp
```

## References
* [Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf)
* [Succinct Data Structures talk by Edward Kmett](https://www.youtube.com/watch?v=uA0Z7_4J7u8)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)
* [Conduit Overview](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview)


## Special mentions
* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)
