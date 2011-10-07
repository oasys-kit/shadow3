#!/bin/tcsh


#Source Opteron2 and compile

echo
echo "Script to set the correct environment for the Makefile in ESRF/NICE computers.   "
echo "Arguments passed directly to Makefile. Only first 7 arguments taken into account."
echo 
echo "---------------------------------------------------------------------------------"
source /scisoft/ESRF_sw/opteron2/set_environment.tcsh
echo -n "gfortran: "
gfortran --version | grep "(GCC)"
echo -n "mpif90: "
mpif90 --version |grep "(GCC)"
echo "---------------------------------------------------------------------------------"

#make $1 $2 $3 $4 $5 $6 $7
make -B MPI=1 shadow3 lib examples
