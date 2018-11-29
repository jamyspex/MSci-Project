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
