#!/bin/bash

# TRACELENS="10 100 1000 10000 100000 1000000 10000000"
TRACELENS="32 316 3162 31622 316227"
PUMPS=$(seq 10 10 700)
TESTTYPES="PeriodWidth SmoothPeriodWidth PeriodHeight SmoothPeriodHeight"

for TESTTYPE in $TESTTYPES
do
  echo % $TESTTYPE
  echo '\addplot table[y expr=\thisrowno{1}/1024/1024] {'
  for i in $TRACELENS
  do
    # ./runTest.sh WindowTrueHeight 10 $i
    # ./runTest.sh WindowTrueWidth 10 $i
    # ./runTest.sh PeriodHeight 10 $i
    echo $i $(./runTest.sh $TESTTYPE 10 $i)
  done
  echo '};'
  echo
done
