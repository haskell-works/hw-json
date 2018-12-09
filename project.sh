#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  install)
    cabal new-install \
      --test --no-run-tests --bench --no-run-benchmarks \
      $CABAL_FLAGS "$@"
    ;;

  build)
    cabal new-build all \
      --disable-tests --disable-benchmarks \
      $CABAL_FLAGS "$@"
    ;;

  test)
    cabal new-test \
      $CABAL_FLAGS "$@"
    ;;

  bench)
    cabal new-bench \
      $CABAL_FLAGS "$@"
    ;;

  repl)
    cabal new-repl \
      $CABAL_FLAGS "$@"
    ;;
esac

