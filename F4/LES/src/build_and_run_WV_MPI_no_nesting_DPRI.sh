#!/bin/sh  


procPerRow=$1
procPerCol=$2

export FC="/usr/local/mpich2-1.4.1p1-gnu/bin/mpif90"
rm ./les_main_mpi
scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_baseline.h 
#scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=1 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} $3

#scons  -f SConstruct.WV v=0            wv_debug=1              nested=1 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol}

mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_LES_baseline_1.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_baseline.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_LES_baseline_2.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_MPI_NEW_WV.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_LES_MPI_NEW_WV_1.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_MPI_NEW_WV.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_LES_MPI_NEW_WV_2.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_MPI_NEW_WV_NEW.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_LES_MPI_NEW_WV_NEW_1.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_MPI_NEW_WV_NEW.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_LES_MPI_NEW_WV_NEW_2.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_WV_NEW.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_WV_NEW_1.txt

scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 nested=0 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} m=macros_mpi_WV_NEW.h
mpiexec -n $((procPerRow*procPerCol))  ./les_main_mpi > eval_WV_NEW_2.txt


