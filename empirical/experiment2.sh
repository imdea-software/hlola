#!/bin/bash

PUMPS=$(seq 10 10 700)
TESTTYPES="PeriodHeight SmoothPeriodHeight"

for TESTTYPE in $TESTTYPES
do
  echo % $TESTTYPE
  echo '\addplot table[y expr=\thisrowno{1}/1024/1024] {'
  for i in $PUMPS
  do
    echo $i $(./runTest.sh $TESTTYPE $i 1000)
  done
  echo '};'
  echo
done
