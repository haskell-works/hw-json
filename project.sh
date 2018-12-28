#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  install)
    cabal new-install -j4 \
      --test --no-run-tests --bench --no-run-benchmarks \
      $CABAL_FLAGS "$@"
    ;;

  build)
    cabal new-build all -j4 \
      --disable-tests --disable-benchmarks \
      $CABAL_FLAGS "$@"
    ;;

  test)
    cabal new-test -j4 --enable-tests --disable-documentation \
      $CABAL_FLAGS "$@"
    ;;

  bench)
    cabal new-bench -j4 \
      $CABAL_FLAGS "$@"
    ;;

  repl)
    cabal new-repl \
      $CABAL_FLAGS "$@"
    ;;

  clean)
    cabal new-clean
    ;;
  
  *)
    echo "Unrecognised command: $cmd"
    exit 1
    ;;
esac

