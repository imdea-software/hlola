#!/bin/bash

FILES_IN="./data/inputs/XXXX1/*"
FILES_OUT_CSV="./data/csv"
FILES_OUT_STAT="./data/csv_stat"

for f in $FILES_IN
do
    echo "Processing $f file..."

    filename_f="$(basename -- $f)"

    CSV_FILENAME_FLOWS="$FILES_OUT_CSV"/"$filename_f".csv
    CSV_FILENAME_FLOWS_TMP="$FILES_OUT_CSV"/"$filename_f".csvtmp
    CSV_FILENAME_STAT="$FILES_OUT_STAT"/"$filename_f".csv
    
    nfdump -r "$f" -o csv > $CSV_FILENAME_FLOWS_TMP

    nfdump -r "$f" -s srcip/bps -s dstip/bps  ' port 0' -o csv > $CSV_FILENAME_STAT
    
    awk -F, 'BEGIN {OFS=","} {print $1,$2,$4,$5,$6,$7,$8,$12,$13,$16,$17,$18,$19}' $CSV_FILENAME_FLOWS_TMP > $CSV_FILENAME_FLOWS

    rm -rf $CSV_FILENAME_FLOWS_TMP
    
    # cat "$f"
done








