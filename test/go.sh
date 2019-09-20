#!/bin/sh

for filename in `ls each*5.f90`
do
gfortran -fdefault-integer-8 ${filename}
./a.out
rm a.out
done
