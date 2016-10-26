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
          54 import           Control.DeepSeq
          64 import           Data.Aeson
          64 import qualified Data.ByteString.Lazy as BSL
          66 json78m <- BSL.readFile "../data/78mb.json"
         167 let !x = deepseq json78m json78m
        1207 let !y = decode json78m :: Maybe Value


### Parsing large Json files in Haskell with hw-json

    Mem (MB) CMD
    -------- ---------------------------------------------------------
          57 import           Foreign
          58 import           Control.Monad
          59 import qualified Data.ByteString as BS
          76 import qualified Data.Vector.Storable as DVS
          77 import           Data.Word
          77 import           HaskellWorks.Data.BalancedParens.Simple
          77 import           HaskellWorks.Data.Bits.BitShown
          77 import           HaskellWorks.Data.FromForeignRegion
          81 import           HaskellWorks.Data.Json.Succinct.Cursor
          82 import           HaskellWorks.Diagnostics
          83 import           System.IO.MMap
          88 (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr "../data/78mbs.json" ReadOnly Nothing
         276 cursor <- measure (fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))

## Examples

### Performance example
(This section out of date)

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
let cd = TC.depth
let ss = TC.subtreeSize
let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
cursor
fc cursor
(fc >=> ns) cursor
```

### Querying example

```
import           Control.Monad
import qualified Data.DList as DL
import           Data.Function
import           Data.List
import           HaskellWorks.Data.Json.LoadCursor
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.MQuery
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Row
import           HaskellWorks.Diagnostics
```

```
!cursor <- loadJsonPartial "../data/78mb.json"
!cursor <- loadJsonWithIndex "../data/78mb.json"
!cursor <- loadJson "../data/78mb.json"
!cursor <- loadJsonWithPoppy512SMinMaxIndex "../data/78mb.json"
let !json = jsonPartialJsonValueAt cursor
let q = MQuery (DL.singleton json)
```

```
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

```
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

```
!cursor <- loadJsonPartial "../data/78mb.json"
!cursor <- loadJsonWithIndex "../data/78mb.json"
!cursor <- loadJson "../data/78mb.json"
!cursor <- loadJsonWithPoppy512SMinMaxIndex "../data/78mb.json"
let !json = lightJsonAt cursor
let q = MQuery (DL.singleton json)
```

```
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


 :set +s
