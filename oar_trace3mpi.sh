#!/bin/bash

#
# script to run trace3mpi using OAR in the ESRF environment
#
# Author: Dimitris Karkoulis.
#

if [ "$1" = "" ]; then
  echo "Usage oar_trace3mpi.sh -a|t|s (NUMPROCS)"
  exit 2
fi


#source Mirone environment
source /scisoft/ESRF_sw/opteron2/set_environment

#
export PATH=.:${PATH}
export LD_LIBRARY_PATH=.:${LD_LIBRARY_PATH}

#get machine file
#cat /var/lib/oar/${OAR_ARRAY_ID} > machines_oar.def

export I_MPI_JOB_CONTEXT=`hostname`_$RANDOM

if [ "$2" = "" ]; then
  nhosts=`wc -l $OAR_NODEFILE | gawk '{print $1}'`
else
  nhosts=$2 
fi

echo "Number of processors to be used: $nhosts"

#
mpirun --prefix /scisoft/ESRF_sw/opteron2 -mca plm_rsh_agent oarsh -machinefile $OAR_NODEFILE  -n $nhosts trace3mpi $1
