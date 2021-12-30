#!/bin/bash
i=$1
for d in ./id*; do
    let "i=i+1"
    mv $d crash_$i
done
