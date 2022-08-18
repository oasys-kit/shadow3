rm -f star.01 begin.dat *.gpl
../shadow3 < shadow3.inp
../shadow3 < plotxy.inp
gnuplot plotxy.gpl

