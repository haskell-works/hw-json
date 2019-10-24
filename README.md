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

```haskell
-- CMD                                                     -- Mem (MB)
---------------------------------------------------------- -- --------
import Control.DeepSeq                                     --      345
import Data.Aeson                                          --      371
import qualified Data.ByteString.Lazy as LBS               --      376
!bs <- LBS.readFile "corpus/bench/hospitalisation.json"    --      380
let !y = decode bs :: Maybe Value                          --      928
```

### Parsing large Json files in Haskell with hw-json

```haskell
-- CMD                                                                -- Mem (MB)
--------------------------------------------------------------------- -- --------
import qualified HaskellWorks.Data.ByteString                as BS    --      351
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF   --      353

!jsonBs <- BS.mmap "corpus/bench/hospitalisation.json"                --      355
let !ibip = JCF.simdToIbBp jsonBs                                     --      358
let !c    = JCF.fromBsIbBp jsonBs ibip                                --      495
```

## Examples

### Navigation example

```haskell
import Control.Monad

import qualified Data.ByteString                             as BS
import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as JCF
import qualified HaskellWorks.Data.TreeCursor                as TC

let fc = TC.firstChild
let ns = TC.nextSibling
let pn = TC.parent
let ss = TC.subtreeSize
let jsonBs  = "[null, {\"field\": 1}]" :: BS.ByteString
let ibip    = JCF.simdToIbBp jsonBs
let cursor  = JCF.fromBsIbBp jsonBs ibip
fc cursor
(fc >=> ns) cursor
```

### Querying example

```haskell
import Control.Monad
import Data.Function
import Data.List
import HaskellWorks.Data.Json.PartialValue
import HaskellWorks.Data.Json.Standard.Cursor.Load.Cursor
import HaskellWorks.Data.Json.Standard.Load.Partial
import HaskellWorks.Data.MQuery
import HaskellWorks.Data.MQuery.Micro

import qualified Data.DList as DL

!cursor <- loadPartial "corpus/bench/78mb.json"
!cursor <- loadCursorWithIndex "corpus/bench/78mb.json"
!cursor <- loadCursor "corpus/bench/78mb.json"
!cursor <- loadCursorWithCsPoppyIndex "corpus/bench/78mb.json"
let !json = jsonPartialJsonValueAt cursor
let q = MQuery (DL.singleton json)

putPretty $ q >>= item & limit 10
putPretty $ q >>= item & page 10 1
putPretty $ q >>= item >>= hasKV "founded_year" (JsonPartialNumber 2005) & limit 10
putPretty $ q >>= item >>= entry
putPretty $ q >>= item >>= entry >>= named "name" & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code")
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code") & onList (uniq . sort)
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount") & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & aggregate sum

putPretty $ q >>= item & limit 10
putPretty $ q >>= item & page 10 1
putPretty $ q >>= item >>= entry
putPretty $ q >>= item >>= entry >>= named "name" & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString)
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString) & onList (uniq . sort)
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount") & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & limit 10
putPretty $ q >>= (item >=> entry >=> named "acquisition" >=> having (entry >=> named "price_currency_code" >=> asString >=> valueOf "USD") >=> entry >=> named "price_amount" >=> castAsInteger ) & aggregate sum
```

## References

* [Semi-Indexing Semi-Structured Data in Tiny Space](http://www.di.unipi.it/~ottavian/files/semi_index_cikm.pdf)
* [Succinct Data Structures talk by Edward Kmett](https://www.youtube.com/watch?v=uA0Z7_4J7u8)
* [Typed Tagless Final Interpreters](http://okmij.org/ftp/tagless-final/course/lecture.pdf)

## Special mentions

* [Sydney Paper Club](http://www.meetup.com/Sydney-Paper-Club/)
