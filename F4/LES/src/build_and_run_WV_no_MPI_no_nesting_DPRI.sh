#!/bin/sh  


procPerRow=1
procPerCol=1

#export FC="/usr/local/mpich2-1.4.1p1-gnu/bin/mpif90"
export FC=$DEVTOOLSETROOT/usr/bin/gfortran 
rm ./les_main
scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=0 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_no_mpi.h 
mpiexec -n $((procPerRow*procPerCol))  ./les_main > eval_LES_baseline_1.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=0 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_no_mpi.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main > eval_LES_baseline_2.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=0 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_no_mpi_WV_NEW.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main > eval_WV_NEW_1.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=0 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_no_mpi_WV_NEW.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main > eval_WV_NEW_2.txt

