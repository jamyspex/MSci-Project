/*=========================================================================
Custom kernel include file generated for this run:
	build-current
=========================================================================*/
#ifndef _ENUMERATIONS_H_
#define _ENUMERATIONS_H_
// -------------------------------------
// Enumerated types 
// -------------------------------------

//for LOOPING
#define API     0
#define KERNEL  1

//for NESTING (only applies when KERNEL looping)
#define FLAT_LOOPING   0
#define NESTED_LOOPING 1

//for WORD
#define INT     0
#define FLOAT   1
#define DOUBLE  2

//target boards/flows
//Note that CPU is an "OpenCL CPU" target (TODO: rename this to CPU_OCL or something)
//If we want a "pure" software implementation without CPU, then define TARGET = HOST
#define AOCL        0
#define AOCL_CHREC  1
#define SDACCEL     2
#define GPU         3
#define CPU         4
#define MAXELER     5
#define HOST        6

//for loop unroll
//since we can expect UNROLL never to be 
//explicitly defined as 0, we use this value to 
//indicate FULL unroll
#define UNROLL_FULL 0

//type of kernel 
#define SINGLEWI 0
#define NDRANGE  1

#endif

 //===============================================
 // Custom parameter definitions for this run 
 //===============================================
 #define TARGET                  AOCL     
 #define WORD                    FLOAT   
 #define VECTOR_SIZE             1 
 #define ROWS                    20 
 #define COLS                    20 
 #define NTOT                    1000 
 #define NTIMES                  1 
 #define WORKITEMS               SINGLEWI 
 #define NUM_SIMD_ITEMS          1   
 #define NUM_COMPUTE_UNITS       1
 #define USECHANNELS             1    
 

#ifndef _DERIVED_PARAMETERS_H_
#define _DERIVED_PARAMETERS_H_

// ----------------------------------------------------
// Derived parameters - common for host and device
// ----------------------------------------------------

#if WORD==INT 
  #define stypeHost int
  #if   VECTOR_SIZE==1
    #define stypeDevice int
  #elif VECTOR_SIZE==2
    #define stypeDevice int2
  #elif VECTOR_SIZE==4
    #define stypeDevice int4
  #elif VECTOR_SIZE==8
    #define stypeDevice int8
  #elif VECTOR_SIZE==16
    #define stypeDevice int16
  #else
    #error Illegal VECTOR_SIZE
  #endif


//NOTE: That I am (should be) using cl_float instead of float (for host)
#elif WORD==FLOAT
  #define stypeHost float
  #if   VECTOR_SIZE==1
    #define stypeDevice float
  #elif VECTOR_SIZE==2
    #define stypeDevice float2
  #elif VECTOR_SIZE==4
    #define stypeDevice float4
  #elif VECTOR_SIZE==8
    #define stypeDevice float8
  #elif VECTOR_SIZE==16
    #define stypeDevice float16
  #else
    #error Illegal VECTOR_SIZE
  #endif

#elif WORD==DOUBLE
  #define stypeHost double
  #if   VECTOR_SIZE==1
    #define stypeDevice double
  #elif VECTOR_SIZE==2
    #define stypeDevice double2
  #elif VECTOR_SIZE==4
    #define stypeDevice double4
  #elif VECTOR_SIZE==8
    #define stypeDevice double8
  #elif VECTOR_SIZE==16
    #define stypeDevice double16
  #else
    #error Illegal VECTOR_SIZE.
  #endif

#else
  #error Illegal data type used for WORD
#endif


//#define COLS   ROWS
#define SIZE   (ROWS*COLS)

//is this an OpenCL implementation or not (i.e. may be Maxeler, pure host)
#if (TARGET==CPU) || (TARGET==AOCL) || (TARGET==AOCL_CHREC) || (TARGET==SDACCEL) || (TARGET==GPU)
#define OCL
#endif

#endif