#!/bin/sh

for filename in `ls *1.f90`
do
gfortran ${filename}
./a.out
rm a.out
done
