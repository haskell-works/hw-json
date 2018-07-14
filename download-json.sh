#!/usr/bin/env bash

echo "Downloading corpus/english-historical-events.json"
curl -s http://www.vizgr.org/historical-events/search.php\?format\=json\&begin_date\=-3000000\&end_date\=20151231\&lang\=en \
  | pv -t -e -b -a > corpus/english-historical-events.json

