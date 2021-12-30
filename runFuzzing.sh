#!/bin/bash
export PATH=$PATH:/opt/GNAT/2018/libexec/gcc/x86_64-pc-linux-gnu/7.3.1/
export PATH=$PATH:/opt/GNAT/2018/libexec/gprbuild/
export PATH=$PATH:/opt/GNAT/2018/bin/
#gprclean -r main.gpr
gprbuild --compiler-subst=Ada,/opt/afl/afl-2.51b/afl-gcc -f -p -P main.gpr
ulimit -s unlimited

rm -fr output/*

gnome-terminal --window-with-profile=afl -- bash -c "ulimit -s unlimited;AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=ON /opt/afl/afl-2.51b/afl-fuzz -m 1024 -i input -o output -S f01 ./bin/compiler @@"
gnome-terminal --window-with-profile=afl -- bash -c "ulimit -s unlimited;AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=ON /opt/afl/afl-2.51b/afl-fuzz -m 1024 -i input -o output -S f02 ./bin/compiler @@"
gnome-terminal --window-with-profile=afl -- bash -c "ulimit -s unlimited;AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=ON /opt/afl/afl-2.51b/afl-fuzz -m 1024 -i input -o output -S f03 ./bin/compiler @@"

AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=ON /opt/afl/afl-2.51b/afl-fuzz -m 1024 -i input -o output -M f00 ./bin/compiler @@ 

