#!/bin/sh  

procPerRow=4
procPerCol=4

export FC="/opt/app/openmpi/2.0.1/gnu-4.8/bin/mpif90"
#export FC="/opt/app/intel/impi/2017.2.174/intel64/bin/mpiifort"

#scons ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol}

#-- wv_debug
scons wv_debug=1 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol}

tssrun -W 24:00  -A p=$((procPerRow*procPerCol)) mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi

