#!/bin/sh  

rm ./les_main

FC=$DEVTOOLSETROOT/usr/bin/gfortran scons -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi_new_wv=0 ocl=0 mpi=0 procPerRow=1 procPerCol=1 $*

time ./les_main
