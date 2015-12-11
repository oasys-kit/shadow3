#!/bin/bash

#
# script to run trace3mpi in the ESRF environment
#
# Author: Dimitris Karkoulis.
#


if [ "$1" = "" ]; then
  echo "Usage mpi_trace3mpi.sh NUMPROCS -a|t|s"
  exit 2
fi

#source Mirone environment
source /scisoft/ESRF_sw/opteron2/set_environment

#
export PATH=.:${PATH}
export LD_LIBRARY_PATH=.:${LD_LIBRARY_PATH}

#
mpirun --prefix /scisoft/ESRF_sw/opteron2 --mca plm_rsh_agent ssh -machinefile mpi_machines.def  -n $1 trace3mpi $2 $3
