#!/bin/bash

echo $@ >> logSandwich.txt

# python3 ./process_netflow.py --sampling 10 $@
python3 ./Tezos/fxgetter.py $@
