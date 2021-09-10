#!/bin/sh
if [ -z "$1" ]; then
    echo "usage sh run.sh N"
    exit 1
fi;
echo "running $1 tests..."
for i in $(seq 1 $1); do
    python generate_exp.py
    gcc test1_1.c -o test1_1
    ./test1_1 > test1_1.txt
    ./../../bin/main test1.c
    ./../../../machine/msm/msm test1.asm > test1.txt

    if ! cmp --silent -- "test1.txt" "test1_1.txt"; then
        echo "error"
      exit 1
    fi
done;
rm test1*
echo "ok"
