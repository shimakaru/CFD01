#!/bin/sh
echo "結果ファイル名を指定してください。名前にBC等を記載しておくと後でわかりやすいです。"
read bcname
rm comparison.txt
rm *.dat
for shellset_mesh_num in 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
do
	for filename in `ls test*.f90`
	do
	cat ${filename}|sed -e s/shellset_mesh_num/${shellset_mesh_num}/g > temp.f90
	gfortran temp.f90
	#echo ${filename} >> comparison.txt
	#./a.out >> comparison.txt
	./a.out >> ${filename}_gnuplot.dat
	rm temp.f90
	rm a.out
	done
done
#グラフ作成
gnuplot << EOF
  set terminal png
  set terminal png font "MigMix 2M,14"
  set output '${bcname}.png'
  set xlabel '反復回数'
  set ylabel '計算格子の数'
  plot "test.f90_gnuplot.dat" u 1:4 with line,"test01(kai).f90_gnuplot.dat" u 1:4 with line
EOF
#結果ファイル退避
mkdir ${bcname}
cp test*.f90 ${bcname}
cp test*gnuplot.dat ${bcname}
cp ${bcname}.png ${bcname}