#!/usr/bin/env bash

case $1 in
  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-json:bmi2
    ;;
  test)
    stack test \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-json:bmi2
    ;;

  bench)
    stack bench \
      --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-json:bmi2
    ;;
esac
