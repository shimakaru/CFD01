#!/bin/sh
rm comparison.txt
for shellset_mesh_num in 4 6 8 10 12 14 16 18 20 25 30 35 40 45 50
do
	for filename in `ls test*.f90`
	do
	cat ${filename}|sed -e s/shellset_mesh_num/${shellset_mesh_num}/g > temp.f90
	gfortran temp.f90
	echo ${filename} >> comparison.txt
	./a.out >> comparison.txt
	rm temp.f90
	rm a.out
	done
done
