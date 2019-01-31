# hw-json
[![master](https://circleci.com/gh/haskell-works/hw-json/tree/master.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-json/tree/master)

`hw-json` is a succinct JSON parsing library.

It uses succinct data-structures to allow traversal of large JSON strings with minimal memory overhead.

For an example, see [`app/Main.hs`](../master/app/Main.hs)

## Prerequisites

* `cabal` version `2.2` or later

## Memory benchmark

### Parsing large Json files in Scala with Argonaut

```text
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
```

### Parsing large Json files in Haskell with Aeson

```text
Mem (MB) CMD
-------- ---------------------------------------------------------
      94 import Control.DeepSeq
     100 import Data.Aeson
     104 import qualified Data.ByteString.Lazy as BSL
     105 bs <- BSL.readFile "../corpus/bench/hospitalisation.json"
     146 let !x = deepseq bs bs
     669 let !y = decode json78m :: Maybe Value
```

### Parsing large Json files in Haskell with hw-json

```text
Mem (MB) CMD
-------- ---------------------------------------------------------
      93 import Foreign
      95 import Control.Monad
      96 import Data.Word
      97 import HaskellWorks.Data.BalancedParens.Simple
      98 import HaskellWorks.Data.Bits.BitShown
      99 import HaskellWorks.Data.FromForeignRegion
     106 import HaskellWorks.Data.Json.Backend.Standard.Cursor
     109 import System.IO.MMap
     110 import qualified Data.ByteString                              as BS
     111 import qualified Data.Vector.Storable                         as DVS
     112 import qualified HaskellWorks.Data.ByteString                 as BS
     114 import qualified HaskellWorks.Data.Json.Backend.Standard.Fast as FAST
     115 bs <- BS.mmap "../corpus/bench/hospitalisation.json"
     203 let !cursor = FAST.makeCursor bs
```

## Examples

### Performance example
(This section out of date)

```haskell
import Foreign
import qualified Data.Vector.Storable as DVS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import System.IO.MMap
import Data.Word
import System.CPUTime
(fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/78mbs.json" ReadOnly Nothing
cursor <- measure (fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64)))
let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
x <- measure $ jsonBsToInterestBs bs
let !y = BS.concat (unescape' "" [bs])

import Foreign
import qualified Data.Vector.Storable as DVS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import System.IO.MMap
import Data.Word
import System.CPUTime
(fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "/Users/jky/Downloads/part40.json" ReadOnly Nothing
let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
x <- measure $ BS.concat $ blankedJsonToInterestBits $ blankJson [bs]
x <- measure $ jsonBsToInterestBs bs

jsonTokenAt $ J.nextSibling $ J.firstChild $ J.nextSibling $ J.firstChild $ J.firstChild  cursor
```

### Navigation example

```haskell
import qualified Data.ByteString                             as BS
import           Data.String
import qualified Data.Vector.Storable                        as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor      as C
import           HaskellWorks.Data.Json.Token
import           HaskellWorks.Data.BalancedParens.Simple
import           HaskellWorks.Data.RankSelect.Base.Rank0
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.RankSelect.Base.Select1
import           HaskellWorks.Data.RankSelect.Poppy512
import qualified HaskellWorks.Data.TreeCursor as TC
import           System.IO.MMap
let fc = TC.firstChild
let ns = TC.nextSibling
let pn = TC.parent
let ss = TC.subtreeSize
let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word64) (SimpleBalancedParens (DVS.Vector Word64))
cursor
fc cursor
(fc >=> ns) cursor
```

### Querying example

```haskell
import Control.Monad
import Data.Function
import Data.List
import HaskellWorks.Data.Json.Backend.Standard.Load
import HaskellWorks.Data.Json.LoadCursor
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro
import HaskellWorks.Data.MQuery.Row
import HaskellWorks.Diagnostics

import qualified Data.DList as DL
```

```haskell
!cursor <- loadJsonPartial "../data/78mb.json"
!cursor <- loadJsonWithIndex "../data/78mb.json"
!cursor <- loadJson "../data/78mb.json"
!cursor <- loadJsonWithCsPoppyIndex "../data/78mb.json"
let !json = jsonPartialJsonValueAt cursor
let q = MQuery (DL.singleton json)
```

```haskell
measureIO $ putPretty $ q >>= item & limit 10
measureIO $ putPretty $ q >>= item & page 10 1
measureIO $ putPretty $ q >>= item >>= hasKV "founded_year" (JsonPartialNumber 2005) & limit 10
measureIO $ putPretty $ q >>= item >>= entry
measureIO $ putPretty $ q >>= item >>= entry >>= named "name" & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code")
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code") & onList (uniq . sort)
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount") & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & aggregate sum
```

```haskell
import           Control.Monad
import qualified Data.DList as DL
import           Data.Function
import           Data.List
import           HaskellWorks.Data.Json.LoadCursor
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.MQuery
import           HaskellWorks.Data.Json.LightJson
import           HaskellWorks.Data.Row
import           HaskellWorks.Diagnostics
```

```haskell
!cursor <- loadJsonPartial "../data/78mb.json"
!cursor <- loadJsonWithIndex "../data/78mb.json"
!cursor <- loadJson "../data/78mb.json"
!cursor <- loadJsonWithCsPoppyIndex "../data/78mb.json"
let !json = lightJsonAt cursor
let q = MQuery (DL.singleton json)
```

```haskell
measureIO $ putPretty $ q >>= item & limit 10
measureIO $ putPretty $ q >>= item & page 10 1
measureIO $ putPretty $ q >>= item >>= entry
measureIO $ putPretty $ q >>= item >>= entry >>= named "name" & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString)
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString) & onList (uniq . sort)
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount") & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & limit 10
measureIO $ putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & aggregate sum
```

### Decoding

#### Line separated base 64 encoded gzipped json

```bash
while read in; do echo "$in" | base64 --decode | gunzip; echo ""; done < file.lgz > firehose.json
```

### Profiling with stack traces

```bash
mafia build -p
cabal repl --ghc-options='-fexternal-interpreter -prof'
```

```haskell
import HaskellWorks.Data.Succinct.BalancedParens
import HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import HaskellWorks.Data.Positioning
import qualified Data.Vector.Storable as DVS
import HaskellWorks.Data.IndexedSeq
(jsonBS, jsonIb, jsonBp) <- loadRawWithIndex "firehose.json"
let bp1 = SimpleBalancedParens jsonBp
let bp2 = SimpleBalancedParens (makePoppy512 jsonBp)
let bp3 = makePoppy512 jsonBp
```

## References

* [Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf)
* [Succinct Data Structures talk by Edward Kmett](https://www.youtube.com/watch?v=uA0Z7_4J7u8)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)

## Special mentions

* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)
