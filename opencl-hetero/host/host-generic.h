#ifndef _HOST_GENERIC_H_
#define _HOST_GENERIC_H_

// ======================================================
// Generic header file for tytra het' opencl framework 
// Primary target is FPGA, but also meant to work with 
// GPUs and CPUs
// By: Syed Waqar Nabi, Glasgow
// 2016.12.05
// ======================================================

// =========================
// Generic inludes
// =========================
// include common file used for enumerations in both host and device
#include "../common/enumerations.h"

//What is the target board/flow?
//[AOCL*/SDACCEL/GPU/CPU]
#ifndef TARGET
  #define TARGET HOST
#endif

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <assert.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <float.h>

//TODO: remove c++ dependancy
#if (TARGET!=MAXELER)
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <cstring>
#include <cassert>
//
#include <fstream>
#include <vector>
#include <cmath>
//#include <cfloat>



// =========================
// OpenCL
// =========================
#include <CL/opencl.h>
#endif

// =========================
// Commonly used Macros
// =========================
# define HLINE "-------------------------------------------------------------\n"
# define SLINE "*************************************************************\n"

# ifndef MIN
# define MIN(x,y) ((x)<(y)?(x):(y))
# endif
# ifndef MAX
# define MAX(x,y) ((x)>(y)?(x):(y))
# endif

// =====================================
// Application 
// =====================================

// -------------------------------------
// Default Benchmark Paramters
// -------------------------------------

// We *must* ensure that we protect any 
// previous definitin of macros, which should
// supercede the following defaults

//#define USECHANNELS

//#define EPSILON 0.00001
//#define EPSILON 0.0001
#define EPSILON 0.1
//#define EPSILON 0.1
//Is this too random? [used for comparing floats]
#ifndef NTOT
  #define NTOT 1000
#endif
  
#ifndef NTIMES
  #define NTIMES 10
#endif

#ifdef WORKITEMS
  #if (WORKITEMS==SINGLEWI)
    #define SINGLEWI_KERNEL
  #elif (WORKITEMS==NDRANGE)
    #define NDRANGE_KERNEL
  #endif  
#else
  #define NDRANGE_KERNEL
#endif

//What is the target board/flow?
//[AOCL*/SDACCEL/GPU/CPU]
#ifndef TARGET
  #define TARGET HOST
#endif

//The type of word used in the streaming experiments 
//[int / float* / double]
#ifndef WORD
  #define WORD FLOAT
#endif

//The size of vector used in the kernel arguments 
//[1 / 2 / 4 / 8 / 16]
#ifndef VECTOR_SIZE
  #define VECTOR_SIZE 1
#endif

//The size of array(s) along one dimensions in a 2D square matrix
#ifndef ROWS
  #define ROWS 53
#endif

#ifndef COLS
  #define COLS ROWS
#endif

//UNROLL_FACTOR, if not defined, should remain undefined
//included here just to indicate the fact that this MACRO is a parameter
#ifndef UNROLL_FACTOR
#endif

//The value of optimziation attribute reqd_work_group_size. Should remain undefined if not defined
#ifndef REQ_WORKGROUP_SIZE
//  #define REQ_WORKGROUP_SIZE 1,0,0
//  #define REQ_WORKGROUP_SIZE 1,1,1
#endif


// -------------------------------------
// KERNELS
// -------------------------------------
#ifdef SINGLEWI_KERNEL
  #ifdef USECHANNELS
    #define NKERNELS 9
    #define K_MEM_RD  				      0
    #define K_SMACHE_MEMRD_2_DYN1	  1
    #define K_DYN1    				      2
    #define K_SMACHE_DYN1_2_DYN2	  3
    #define K_DYN2    				      4
    #define K_SMACHE_DYN2_2_SHAPIRO	5
    #define K_SHAPIRO 				      6
    #define K_UPDATES 				      7
    #define K_MEM_WR  				      8
  #else  
    #define NKERNELS 1
  #endif
#endif  

#ifdef NDRANGE_KERNEL
  #define NKERNELS  4
  #define K_DYN1    0
  #define K_DYN2    1
  #define K_SHAPIRO 2
  #define K_UPDATES 3
#endif  

//choice of kernel


// -------------------------------------
// AOCL-specific Parameters
// -------------------------------------
//The value of AOCL optimization attribute NUM_SIMD_ITEMS
//[1 / 2 / 4 / 8 / 16] ??
#ifndef NUM_SIMD_ITEMS
  #define NUM_SIMD_ITEMS 1
#endif

//The value of AOCL optimziation attribute num_compute_units
#ifndef NUM_COMPUTE_UNITS
  #define NUM_COMPUTE_UNITS 1
#endif


// -------------------------------------
// NOVO-G AT CHREC
// -------------------------------------
//Currently testing for 2 devices
#if TARGET==AOCL_CHREC
  #define NDEVICES 2
  #define D0       0
  #define D1       1
#else  
  #define NDEVICES 1
#endif
  
// How many unique "programs" do I have.
// For Novo-G target, it would be the same as the
// number of devices I use, as each device would have a unique
// program
#define NPROGRAMS NDEVICES

//******* Kernel to device mapping ********
//-----------------------------------------
//This only takes care of the host code. On the kernel side,
//you have to make sure that the correct kernel files for each device
//are available
#define D4_K_MEM_RD  				        D0
#define D4_K_SMACHE_MEMRD_2_DYN1	  D0
#define D4_K_DYN1    				        D0
#define D4_K_SMACHE_DYN1_2_DYN2	    D1
#define D4_K_DYN2    				        D1
#define D4_K_SMACHE_DYN2_2_SHAPIRO  D1
#define D4_K_SHAPIRO 				        D1
#define D4_K_UPDATES 				        D1
#define D4_K_MEM_WR  				        D1

// -------------------------------------
// SDACCEL-specific Parameters
// -------------------------------------

// ----------------------------------------------------
// Derived parameters - include common file for host and device
// ----------------------------------------------------
// this cant be included before the default parameters are defined above
#include "../common/derivedParameters.h"


// -------------------------------------
// Memory alignments
// -------------------------------------
#define SDACCEL_ALIGNMENT 64
#define AOCL_ALIGNMENT    64
#define CPU_ALIGNMENT     64
#define GPU_ALIGNMENT     64
#define MAXELER_ALIGNMENT 64

#if (TARGET==CPU) || (TARGET==HOST)
  #define ALIGNMENT CPU_ALIGNMENT
#elif   TARGET==SDACCEL
  #define ALIGNMENT SDACCEL_ALIGNMENT
#elif TARGET==AOCL
  #define ALIGNMENT AOCL_ALIGNMENT
#elif TARGET==AOCL_CHREC
  #define ALIGNMENT AOCL_ALIGNMENT
#elif TARGET==GPU
  #define ALIGNMENT GPU_ALIGNMENT
#elif TARGET==MAXELER
  #define ALIGNMENT MAXELER_ALIGNMENT
#else
  #error Invalid TARGET defined.
#endif


//-------------------------------
//checktick()
//-------------------------------
# define  M 20
int checktick() ;

//-------------------------------
// mysecond()
//-------------------------------

/* A gettimeofday routine to give access to the wall
   clock timer on most UNIX-like systems.  */

#include <sys/time.h>

double mysecond();

#ifndef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))
#endif

//-------------------------------
// CHECK_ERRORS()
//-------------------------------
#define CHECK_ERRORS(ERR, STRING)           \
    if(ERR != CL_SUCCESS)                   \
    {                                       \
      printf("OpenCL error with code %d ::  \
        happened in file %s ::              \
        at line %d ::                       \
        Error Message from Program %s ::    \
        Exiting...\n",                      \
      ERR, __FILE__, __LINE__, STRING);     \
      exit(1);                              \
    }


//-------------------------------
// OCLBASIC_PRINT_TEXT_PROPERTY()
//-------------------------------
#define OCLBASIC_PRINT_TEXT_PROPERTY(NAME) {           \
  /* When we query for string properties, first we */  \
  /* need to get string length:                    */  \
  size_t property_length = 0;                          \
  err = clGetDeviceInfo(                               \
    device_id,                                         \
    NAME,                                              \
    0,                                                 \
    0,                                                 \
    &property_length                                   \
    );                                                 \
  CHECK_ERRORS(err,"");                                \
  /* Then allocate buffer. No need to add 1 symbol */  \
  /* to store terminating zero; OpenCL takes care  */  \
  /* about it:                                     */  \
  char* property_value = new char[property_length];    \
  err = clGetDeviceInfo(                               \
    device_id,                                         \
    NAME,                                              \
    property_length,                                   \
    property_value,                                    \
    0                                                  \
  );                                                   \
  CHECK_ERRORS(err,"");                                \
  printf("%s:\t%s\n", #NAME, property_value );         \
  delete [] property_value;                            \
  } 


#define OCLBASIC_PRINT_NUMERIC_PROPERTY(NAME, TYPE) {  \
  TYPE property_value;                                 \
  size_t property_length = 0;                          \
  err = clGetDeviceInfo(                               \
    device_id,                                         \
    NAME,                                              \
    sizeof(property_value),                            \
    &property_value,                                   \
    &property_length                                   \
  );                                                   \
  assert(property_length == sizeof(property_value));   \
  CHECK_ERRORS(err,"");                                \
  printf("%s:\t%d\n", #NAME, property_value);          \
 }

//----------------------------------------------------
// load_file_to_memory()
//----------------------------------------------------
int load_file_to_memory(const char *filename, char **result);

//----------------------------------------------------
// oclh_init_data()
//----------------------------------------------------
void oclh_init_data(stypeHost* a2d, stypeHost* b2d, stypeHost* c2d, int BytesPerWord);

//----------------------------------------------------
// oclh_timing_setup()
//----------------------------------------------------
void oclh_timing_setup(stypeHost* a2d, int BytesPerWord);

//----------------------------------------------------
// display setup()
//----------------------------------------------------
void oclh_display_setup();



#ifdef OCL
//----------------------------------------------------
// oclh_opencl_boilerplate()
//----------------------------------------------------
int  oclh_opencl_boilerplate ( cl_context*      context_ref
                            , cl_command_queue* commands_ref
                            , cl_program*       program_ref
                            , cl_kernel*        kernel_ref
                            , int               argc
                            , char**            argv
) ;//oclh_opencl_boilerplate

//----------------------------------------------------
// oclh_create_device_buffer()
//----------------------------------------------------
void oclh_create_cldevice_buffer ( cl_mem*     buffer
                                 , cl_context* context
                                 , cl_mem_flags flag
) ;

//----------------------------------------------------
// oclh_blocking_write_cl_buffer()
//----------------------------------------------------
void oclh_blocking_write_cl_buffer ( cl_command_queue* commands
                                  , cl_mem*           dbuffer
                                  , stypeHost*        hbuffer
) ;

//----------------------------------------------------
// oclh_blocking_read_cl_buffer()
//----------------------------------------------------
void oclh_blocking_read_cl_buffer ( cl_command_queue* commands
                                  , cl_mem*           dbuffer
                                  , stypeHost*        hbuffer
) ;

//----------------------------------------------------
// oclh_set_kernel_args()
//----------------------------------------------------
void oclh_set_kernel_args ( cl_kernel* kernels 
                          , stypeHost* dt
                          , stypeHost* dx
                          , stypeHost* dy
                          , stypeHost* g
                          , stypeHost* eps
                          , stypeHost* hmin
                          , cl_mem*    dev_eta
                          , cl_mem*    dev_un
                          , cl_mem*    dev_u
                          , cl_mem*    dev_wet
                          , cl_mem*    dev_v
                          , cl_mem*    dev_vn
                          , cl_mem*    dev_h
                          , cl_mem*    dev_etan
                          , cl_mem*    dev_hzero
//                          , int*       rows
//                          , int*       cols
                          );
//----------------------------------------------------
// oclh_get_global_local_sizes()
//----------------------------------------------------
void oclh_get_global_local_sizes ( size_t* globalSize
                                , size_t* localSize
);

//----------------------------------------------------
// oclh_get_global_local_sizes()
//----------------------------------------------------
void oclh_enq_cl_kernel  ( cl_command_queue* commands
                        , cl_kernel*        kernel
                        , size_t*           globalSize
                        , size_t*           localSize
) ;

#endif
//#ifdef OCL

//----------------------------------------------------
// oclh_log_results
//----------------------------------------------------
void oclh_log_results  ( stypeHost* eta
                      , stypeHost*  h
                      , stypeHost*  h_g
                      , stypeHost*  u
                      , stypeHost*  v
                      , stypeHost*  h0
);

//----------------------------------------------------
// oclh_verify_results()
//----------------------------------------------------
int oclh_verify_results ( stypeHost* h
                        , stypeHost* h_g
);

//----------------------------------------------------
// oclh_calculate_bandwidth()
//----------------------------------------------------
void oclh_calculate_performance(
);

//----------------------------------------------------
// oclh_disp_timing_profile()
//----------------------------------------------------
void oclh_disp_timing_profile();

#endif
