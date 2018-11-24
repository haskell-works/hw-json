#!/usr/bin/env bash

STACK_FLAGS="
  --flag bits-extra:bmi2
  --flag hw-rankselect-base:bmi2
  --flag hw-rankselect:bmi2
"

case $1 in
  install)
    stack install \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS
    ;;

  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS
    ;;

  test)
    stack test \
      $STACK_FLAGS
    ;;

  bench)
    stack bench \
      $STACK_FLAGS
    ;;

  repl)
    stack repl \
      $STACK_FLAGS
    ;;
esac
