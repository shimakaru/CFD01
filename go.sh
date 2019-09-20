#!/bin/sh
rm comparison.txt
for shellset_mesh_num in 3 4 5 6 7 8 9 10 11 12 13 14 15 20 30 40 60 80 100 120 140 160 180 200
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
