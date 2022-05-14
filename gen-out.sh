#!/usr/bin/env bash

for example in tests/good/*.rat; do
  filename=$(basename -- "$example")
  filename="${filename%.*}"
  outfile=tests/good/"${filename}.out"
  errfile=tests/good/"${filename}.err"
  ./interpreter $example 1> $outfile 2> $errfile
done

for example in tests/bad/*.rat; do
  filename=$(basename -- "$example")
  filename="${filename%.*}"
  outfile=tests/bad/"${filename}.out"
  errfile=tests/bad/"${filename}.err"
  ./interpreter $example 1> $outfile 2> $errfile
done
