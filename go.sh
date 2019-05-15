#!/bin/sh

for filename in `ls *3.f90`
do
gfortran ${filename}
./a.out
rm a.out
done
