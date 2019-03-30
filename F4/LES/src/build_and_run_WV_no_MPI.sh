#!/bin/sh  

rm ./les_main

#FC=/opt/local/bin/gfortran-mp-7 
scons -f SConstruct.WV v=0 gr_debug=0 wv_debug=0 mpi=0 procPerRow=1 procPerCol=1 m=./macros.h,./macros_to_skip.h

time ./les_main
