# Environment variables for OpenCL Integration -- modify as required

# Directory containing OpenCLIntegration folder
export OPENCL_DIR=$HOME/Documents/Uni/MSci-Project

# OpenCL SDK paths
# NVIDIA
export NVSDKCUDA_ROOT="/Developer/GPU\ Computing/"
# AMD
export AMDAPPSDKROOT=/opt/AMDAPPSDK-3.0
# Intel
export INTELOCLSDKROOT=/usr
# Altera
export ALTERAOCLSDKROOT=/path/to/altera/oclsdk
export AOCL_BOARD_PACKAGE_ROOT=/path/to/altera/bsp
# Xilinx
# TBA

# Compilers
export CXX_COMPILER=/usr/bin/g++
export CXX=$CXX_COMPILER
export C_COMPILER=/usr/bin/gcc
export CC=$C_COMPILER
# Must be either GNU or PGI compiler, no others supported yet
export FORTRAN_COMPILER=/usr/bin/gfortran
export FC=$FORTRAN_COMPILER

# Don't modify below this line
export OPENCL_GPU=`$OPENCL_DIR/OpenCLIntegration/bin/test_gpu.pl`
export OPENCL_CPU=`$OPENCL_DIR/OpenCLIntegration/bin/test_cpu.pl`
export OPENCL_ACC=`$OPENCL_DIR/OpenCLIntegration/bin/test_acc.pl`

export PYTHONPATH=$PYTHONPATH:$OPENCL_DIR/OpenCLIntegration
export PATH=$OPENCL_DIR/OpenCLIntegration/bin:$PATH


