#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  install)
    cabal new-install \
      --symlink-bindir=$HOME/.local/bin \
      -j8 --overwrite-policy=always --disable-documentation \
      exe:hw-json
      $CABAL_FLAGS "$@"
    ;;

  build)
    cabal new-build all -j8 \
      --disable-tests --disable-benchmarks \
      $CABAL_FLAGS "$@"
    ;;

  test)
    cabal new-test -j8 --enable-tests --disable-documentation \
      $CABAL_FLAGS "$@"
    ;;

  bench)
    cabal new-bench -j8 \
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

