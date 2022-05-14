#!/usr/bin/env bash

tmperr=$(mktemp /tmp/err.XXXXXX)
tmpout=$(mktemp /tmp/out.XXXXXX)

good_passed=0
good_count=0

for example in tests/good/*.rat; do
  echo -e "Running $example:"
  filename=$(basename -- "$example")
  filename="${filename%.*}"
  outfile=tests/good/"${filename}.out"
  errfile=tests/good/"${filename}.err"
  good_res=0
  good_out=0
  good_err=0

  ./interpreter "$example" 1> $tmpout 2> $tmperr

  if [[ $? -eq 0 ]]; then
    ((good_res=good_res+1))
    echo -e "    [return code OK]"
  else
    echo -e "    [return code WA]"
  fi

  if cmp -s $tmpout $outfile; then
    ((good_out=good_out+1))
    echo -e "    [stdout OK]"
  else
    echo -e "    [stdout WA]"
  fi

  if cmp -s $tmperr $errfile; then
    ((good_err=good_err+1))
    echo -e "    [stderr OK]"
  else
    echo -e "    [stderr WA]"
  fi

  if [[ $good_res == 1 && $good_err == 1 && $good_out == 1 ]]; then
    ((good_passed=good_passed+1))
  fi
  ((good_count=good_count+1))
done

bad_passed=0
bad_count=0

for example in tests/bad/*.rat; do
  echo -e "Running $example:"
  filename=$(basename -- "$example")
  filename="${filename%.*}"
  outfile=tests/bad/"${filename}.out"
  errfile=tests/bad/"${filename}.err"
  bad_res=0
  bad_out=0
  bad_err=0

  ./interpreter "$example" 1> $tmpout 2> $tmperr

  if [[ $? -eq 1 ]]; then
    ((bad_res=bad_res+1))
    echo -e "    [return code OK]"
  else
    echo -e "    [return code WA]"
  fi

  if cmp -s $tmpout $outfile; then
    ((bad_out=bad_out+1))
    echo -e "    [stdout OK]"
  else
    echo -e "    [stdout WA]"
  fi

  if cmp -s $tmperr $errfile; then
    ((bad_err=bad_err+1))
    echo -e "    [stderr OK]"
  else
    echo -e "    [stderr WA]"
  fi

  if [[ $good_res == 1 && $good_err == 1 && $good_out == 1 ]]; then
    ((bad_passed=bad_passed+1))
  fi
  ((bad_count=bad_count+1))
done

echo -e "=================================================="
echo -e "=================================================="
echo -e "Passed $good_passed / $good_count execution tests"
echo -e "Passed $bad_passed / $bad_count error tests"