#!/bin/bash

TESTTYPE=$1
PUMP=$2
TRACELEN=$3

# RET=$((echo p,q; for i in $(eval echo "{1..$TRACELEN}"); do echo True,False; done) | HLola $TESTTYPE $PUMP +RTS -t --machine-readable 2>&1 > /dev/null)
#RET=$(HLola $TESTTYPE $PUMP $TRACELEN +RTS -M50m -t --machine-readable 2>&1 > /dev/null)
#echo $RET; exit
RET=$(HLola $TESTTYPE $PUMP $TRACELEN +RTS -M50m -t --machine-readable 2>&1 > /dev/null)
# echo $TESTTYPE $PUMP $TRACELEN
(echo $RET | grep -q "exhausted") && (echo "Low memory"; exit -1)
echo $RET | grep -o "max_bytes_used[^)]*" | sed 's/max_bytes_used", "//;s/"//'
