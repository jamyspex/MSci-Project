# MPI implementation of the LES

This repository contains the code that adds support for MPI in the LES.
Note that the GIS files do not contain the actual GIS data for Kyoto but synthetic data.

# Requirements
- Python 2.7 or a later version of Python 2.x (not Python 3).
- SConstruct (http://www.scons.org/)
- An MPI library (MPICH 3.1.3 and OpenMPI 1.8.4 used during testing).

# Compiling and running the LES

Original single threaded LES:
```shell
scons ocl=0 mpi=0
./les_main
```

MPI LES:

```shell
ROW=X # Number of processes per row
COL=Y # Number of processes per column
TOTAL=$(($ROW*$COL)) # Total number of processes
scons ocl=0 mpi=1 procPerRow=$ROW procPerCol=$COL
mpiexec -np $TOTAL ./les_main_mpi
```
