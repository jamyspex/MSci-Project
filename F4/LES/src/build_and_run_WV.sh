#!/bin/sh  

rm ./les_main_mpi

procPerRow=$1
procPerCol=$2

#export FC=/opt/local/bin/mpifort-openmpi-mp 
export FC=/opt/local/bin/mpifort-mpich-gcc7
scons  -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=1 nested=1 ocl=0 mpi=1 procPerRow=${procPerRow} procPerCol=${procPerCol} $3

#time mpiexec-openmpi-gcc49 -np $((procPerRow*procPerCol)) ./les_main_mpi 
time mpiexec-mpich-gcc7 -np $((procPerRow*procPerCol)) ./les_main_mpi
