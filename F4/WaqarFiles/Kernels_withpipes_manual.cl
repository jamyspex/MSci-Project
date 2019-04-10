// Waqar Nabi, Glasgow, 2019.04.05
//

//un-comment if usign double
//#pragma OPENCL EXTENSION cl_khr_fp64 : enable

//parameters that must be common across host and device are in this header file
// ** NOTE ** that if you make changes to it, Make will recompile host but _not_
// the device. So you must run clean and then make again to ensure device 
// recompiles with these parameters
#include "common_host_dev.h"    

//un-comment if usign double
//#pragma OPENCL EXTENSION cl_khr_fp64 : enable

//If we are using floats or doubles, we use float version of abs (fabs)
//#define ABS abs
#define ABS fabs

#define  ROWS      _ROWS     
#define  COLS      _COLS     
#define  NX        _NX       
#define  NY        _NY       
#define  XMID      _XMID     
#define  YMID      _YMID     
#define  DATA_SIZE _DATA_SIZE
#define  NTOT      _NTOT    

// -------------------------------
// Pipe declarations
// -------------------------------
//if stencil is required on any stream between two kernels, then
//we insert a "smache" module in-between, which requires two versions of each
//pipe... pre and post 
//the post pipe will also contain any additional channels for stencil elements


//MEMRD-2-DYN1
pipe data_t     u_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     v_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     h_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   eta_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  etan_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   wet_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t hzero_memrd_2_dyn1_pre __attribute__((xcl_reqd_pipe_depth(16)));

pipe data_t     u_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     v_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     h_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   eta_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t eta_j_kp1_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t eta_jp1_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  etan_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   wet_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t wet_j_kp1_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t wet_jp1_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t hzero_j_k_memrd_2_dyn1_post __attribute__((xcl_reqd_pipe_depth(16)));

//DYN1-2-DYN2
pipe data_t    un_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t    vn_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     h_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   eta_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  etan_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   wet_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t hzero_dyn1_2_dyn2_pre __attribute__((xcl_reqd_pipe_depth(16)));

pipe data_t    un_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  un_jm1_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  un_j_km1_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t    vn_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  vn_jm1_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  vn_j_km1_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     h_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   h_jm1_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   h_j_km1_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   h_j_kp1_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   h_jp1_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   eta_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  etan_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   wet_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t hzero_j_k_dyn1_2_dyn2_post __attribute__((xcl_reqd_pipe_depth(16)));

//DYN2-2-SHAPIRO
pipe data_t    un_dyn2_2_shapiro_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t    vn_dyn2_2_shapiro_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   eta_dyn2_2_shapiro_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  etan_dyn2_2_shapiro_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   wet_dyn2_2_shapiro_pre __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t hzero_dyn2_2_shapiro_pre __attribute__((xcl_reqd_pipe_depth(16)));

pipe data_t     un_j_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t     vn_j_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t    eta_j_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t   etan_j_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t etan_jm1_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t etan_j_km1_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t etan_j_kp1_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t etan_jp1_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t    wet_j_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  wet_jm1_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  wet_j_km1_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  wet_j_kp1_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  wet_jp1_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  hzero_j_k_dyn2_2_shapiro_post __attribute__((xcl_reqd_pipe_depth(16)));

//SHAPIRO-2-UPDATE
pipe data_t eta_shapiro_2_udpate    __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t un_shapiro_2_udpate     __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t vn_shapiro_2_udpate     __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t h_shapiro_2_udpate      __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t hzero_shapiro_2_udpate  __attribute__((xcl_reqd_pipe_depth(16)));

//UPDATE-2-MEMWR
pipe data_t  u_out_update     __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  v_out_update     __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  h_out_update     __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  eta_out_update   __attribute__((xcl_reqd_pipe_depth(16)));
pipe data_t  wet_out_update   __attribute__((xcl_reqd_pipe_depth(16)));


// ===============================
// KERNELS
// ===============================

//------------------------------------------
// Read memory kernel
//------------------------------------------

kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))
void kernel_mem_rd( __global data_t * restrict u
                       , __global data_t * restrict v
                       , __global data_t * restrict h
                       , __global data_t * restrict eta
                       , __global data_t * restrict etan
                       , __global data_t * restrict wet
                       , __global data_t * restrict hzero
                      ) {
//  for (index=0; index < (DATA_SIZE + COLS); index++) {     
  for (int index=0; index < DATA_SIZE; index++) {     
      data_t u_data     = u[index];
      data_t v_data     = v[index];
      data_t h_data     = h[index];
      data_t eta_data   = eta[index];
      data_t etan_data  = etan[index];
      data_t wet_data   = wet[index];
      data_t hzero_data = hzero[index];

      //#ifdef DEBUG_KERNEL
      //printf("K_MEM-RD-ATTEMPT-WRITE: index = %d\n", index);
      //#endif

      //the write and reads from the pipe must be in the same sequence, synched with memory fences (This was an AOCL requirement)
      write_pipe_block(u_memrd_2_dyn1_pre     ,&u_data   );   
      write_pipe_block(v_memrd_2_dyn1_pre     ,&v_data   );   
      write_pipe_block(h_memrd_2_dyn1_pre     ,&h_data   );   
      write_pipe_block(eta_memrd_2_dyn1_pre   ,&eta_data );   
      write_pipe_block(etan_memrd_2_dyn1_pre  ,&etan_data );  
      write_pipe_block(wet_memrd_2_dyn1_pre   ,&wet_data );   
      write_pipe_block(hzero_memrd_2_dyn1_pre ,&hzero_data );  

      //#ifdef DEBUG_KERNEL
      //printf("kernel_mem_rd: count = index = %d, wet = %f\n", index, wet_data);
      //#endif
//    }
//  }
  }
}

// -------------------------------
// SMACHE_MEMRD_2_DYN1
// -------------------------------
//If there is a stencil/offset required on an stream between 2 kernels,
//then we introduce this "SMACH" module. This makes the design cleaner and more modular,
//and also make it easier to incorporate tytra-smache approach later on
__kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))
void kernel_smache_memrd_2_dyn1 (){   
  //common parameters
  //-----------------
  const int arrsize   = DATA_SIZE; //Size of  input array(s)
  
  //TODO: I should be able to derive these parameters from given offsets, but this should
  // be a compile-time constant, otherwise I will needlessly waste processing on it
  const int ker_maxoffpos     = COLS; 
  const int ker_maxoffneg     = 0;    
  const int nloop             = arrsize + ker_maxoffpos;
  
  const int ker_buffsize  = ker_maxoffpos + ker_maxoffneg + 1;
  //const int ind_j_k       = ker_buffsize - 1 - ker_maxoffneg;
  const int ind_j_k       = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k     = ind_j_k + COLS;  
  const int ind_j_kp1     = ind_j_k + 1;     
  const int ind_jm1_k     = ind_j_k - COLS;  
  const int ind_j_km1     = ind_j_k - 1;     
  
  //buffers
  //-------- 
  data_t     u_buffer [ker_buffsize];
  data_t     v_buffer [ker_buffsize];
  data_t     h_buffer [ker_buffsize];
  data_t   eta_buffer [ker_buffsize];
  data_t  etan_buffer [ker_buffsize];
  data_t   wet_buffer [ker_buffsize];
  data_t hzero_buffer [ker_buffsize];

  //The tuple variables to emit
  data_t     u_j_k;
  data_t     v_j_k;
  data_t     h_j_k;
  data_t   eta_j_k;
  data_t eta_j_kp1;
  data_t eta_jp1_k;
  data_t  etan_j_k;
  data_t   wet_j_k;
  data_t wet_j_kp1;
  data_t wet_jp1_k;
  data_t hzero_j_k;
  
  int count, compindex, count_backup;
  //loop for the entire array + offset buffer
  for (count=0; count < nloop ; count++) {  
  //for (int dcount=0; dcount < nloop*2 ; dcount = dcount + 2) {  
    //count = dcount / 2;
    compindex = count - ker_maxoffpos;

    //count_backup = count; //as count keeps getting corrupted!

    //#ifdef DEBUG_KERNEL
    //printf("kernel_smache_memrd_2_dyn1-READ-A\t: nloop = %d, count = %d\n", 
    //        nloop, count);
    //#endif


    //this unrolled loop implements a SHIFT-RIGHT register for the buffer, which is (should be)
    //more effecient than a naive buffer that is written in a RAM fashion
    #pragma unroll 
     for (int i = 0; i < ker_buffsize-1 ; ++i) {
              u_buffer[i] =     u_buffer[i + 1];
              v_buffer[i] =     v_buffer[i + 1];
              h_buffer[i] =     h_buffer[i + 1];
            eta_buffer[i] =   eta_buffer[i + 1];
           etan_buffer[i] =  etan_buffer[i + 1];
            wet_buffer[i] =   wet_buffer[i + 1];
          hzero_buffer[i] = hzero_buffer[i + 1];
         //#ifdef DEBUG_KERNEL
         //printf("kernel_smache_memrd_2_dyn1: count = %d, wet_buffer[%d] = %f\n", count, i, wet_buffer[i]);
         //#endif
      }    
      
    //#ifdef DEBUG_KERNEL
    //printf("kernel_smache_memrd_2_dyn1-READ-B\t: count = %d\n", count);
    //#endif
      
    //we read into MS-word of buffers until count reaches limit of input array
    if(count < arrsize) {   
     read_pipe_block(u_memrd_2_dyn1_pre    , &    u_buffer[ker_buffsize-1] );  
     read_pipe_block(v_memrd_2_dyn1_pre    , &    v_buffer[ker_buffsize-1] );  
     read_pipe_block(h_memrd_2_dyn1_pre    , &    h_buffer[ker_buffsize-1] );  
     read_pipe_block(eta_memrd_2_dyn1_pre  , &  eta_buffer[ker_buffsize-1] );  
     read_pipe_block(etan_memrd_2_dyn1_pre , & etan_buffer[ker_buffsize-1] );  
     read_pipe_block(wet_memrd_2_dyn1_pre  , &  wet_buffer[ker_buffsize-1] );  
     read_pipe_block(hzero_memrd_2_dyn1_pre, &hzero_buffer[ker_buffsize-1] );  
      
      //#ifdef DEBUG_KERNEL
      //printf("kernel_smache_memrd_2_dyn1: count = %d, wet[%d] = %f\n", count, (ker_buffsize-1), wet_buffer[ker_buffsize-1]);
      //#endif

    //#ifdef DEBUG_KERNEL
    //printf("kernel_smache_memrd_2_dyn1-READ-C\t: count = %d\n", count);
    //#endif
   }//if
    
    //count = count_backup;//un-corrupt
    //#ifdef DEBUG_KERNEL
    //printf("kernel_smache_memrd_2_dyn1-READ-D\t: count = %d\n", count);
    //#endif


    //start emitting required data/stencil when compindex reaches 0
    if(compindex>=0) {
      //#ifdef DEBUG_KERNEL
      //printf("kernel_smache_memrd_2_dyn1-WRITE-A\t: count = %d\n", count);
      //#endif
      //get the data and stencil values from buffer
          u_j_k   =     u_buffer[ind_j_k]; 
          v_j_k   =     v_buffer[ind_j_k];
          h_j_k   =     h_buffer[ind_j_k];
        eta_j_k   =   eta_buffer[ind_j_k];
      eta_j_kp1   =   eta_buffer[ind_j_kp1];
      eta_jp1_k   =   eta_buffer[ind_jp1_k];
       etan_j_k   =  etan_buffer[ind_j_k];
        wet_j_k   =   wet_buffer[ind_j_k];
      wet_j_kp1   =   wet_buffer[ind_j_kp1];
      wet_jp1_k   =   wet_buffer[ind_jp1_k];
      hzero_j_k   = hzero_buffer[ind_j_k];

      //#ifdef DEBUG_KERNEL
      //printf("kernel_smache_memrd_2_dyn1: count = %d, compindex = %d,  wet = %f\n", count, compindex,  wet_j_k);
      //#endif

  
      write_pipe_block (    u_j_k_memrd_2_dyn1_post, &    u_j_k);
      write_pipe_block (    v_j_k_memrd_2_dyn1_post, &    v_j_k);
      write_pipe_block (    h_j_k_memrd_2_dyn1_post, &    h_j_k);
      write_pipe_block (  eta_j_k_memrd_2_dyn1_post, &  eta_j_k);
      write_pipe_block (eta_j_kp1_memrd_2_dyn1_post, &eta_j_kp1);
      write_pipe_block (eta_jp1_k_memrd_2_dyn1_post, &eta_jp1_k);
      write_pipe_block ( etan_j_k_memrd_2_dyn1_post, & etan_j_k);
      write_pipe_block (  wet_j_k_memrd_2_dyn1_post, &  wet_j_k);
      write_pipe_block (wet_j_kp1_memrd_2_dyn1_post, &wet_j_kp1);
      write_pipe_block (wet_jp1_k_memrd_2_dyn1_post, &wet_jp1_k);
      write_pipe_block (hzero_j_k_memrd_2_dyn1_post, &hzero_j_k); 
    }//if(compindex>=0)
  }//for     
}//()

// -------------------------------
// DYN1
// -------------------------------
__kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))
void kernel_dyn1( const data_t dt
                         , const data_t dx
                         , const data_t dy
                         , const data_t g
                         ) {

  //locals
  //-------------------
  data_t du;
  data_t dv;
  data_t uu;
  data_t vv;
  data_t duu;
  data_t dvv;

  const int arrsize   = DATA_SIZE; //Size of  input array(s)
  const int nloop     = arrsize;

  int compindex; 
  int j, k;      

  //input from channels
  //-------------------
  data_t u_j_k     ;
  data_t v_j_k     ;
  data_t h_j_k     ;
  data_t eta_j_k   ;
  data_t eta_j_kp1 ;
  data_t eta_jp1_k ;
  data_t etan_j_k  ;
  data_t wet_j_k   ;
  data_t wet_j_kp1 ;
  data_t wet_jp1_k ;
  data_t hzero_j_k ;


  //loop for the entire array
  for (int count=0; count < nloop ; count++) {  
    compindex = count;
    j = compindex/COLS;
    k = compindex%COLS;

    //#ifdef DEBUG_KERNEL
    //printf("kernel_dyn1-READ-A\t: count = %d\n", count);
    //#endif

    read_pipe_block(    u_j_k_memrd_2_dyn1_post, &u_j_k    );
    read_pipe_block(    v_j_k_memrd_2_dyn1_post, &v_j_k    );
    read_pipe_block(    h_j_k_memrd_2_dyn1_post, &h_j_k    );
    read_pipe_block(  eta_j_k_memrd_2_dyn1_post, &eta_j_k  );
    read_pipe_block(eta_j_kp1_memrd_2_dyn1_post, &eta_j_kp1);
    read_pipe_block(eta_jp1_k_memrd_2_dyn1_post, &eta_jp1_k);
    read_pipe_block( etan_j_k_memrd_2_dyn1_post, &etan_j_k );
    read_pipe_block(  wet_j_k_memrd_2_dyn1_post, &wet_j_k  );
    read_pipe_block(wet_j_kp1_memrd_2_dyn1_post, &wet_j_kp1);
    read_pipe_block(wet_jp1_k_memrd_2_dyn1_post, &wet_jp1_k);
    read_pipe_block(hzero_j_k_memrd_2_dyn1_post, &hzero_j_k); 

    //#ifdef DEBUG_KERNEL
    //printf("kernel_dyn1-READ-B\t: count = %d\n", count);
    //#endif
    
    data_t un_j_k = 0.0;
    data_t vn_j_k = 0.0;
    //exclude boundaries when computing un and vn
    if  ((j>=1) && (k>=1) && (j<= ROWS-2) && (k<=COLS-2)) {      
      //#ifdef DEBUG_KERNEL
      //printf("K_DYN1: l = %d, index = %d, (j,k) = (%d, %d); wet_j_k = %f, wet_jp1_k = %f, wet_j_kp1 = %f \n",l, index, j ,k
      //  , wet_j_k, wet_jp1_k, wet_j_kp1);
      //#endif

      duu  = -dt 
           * g
           * ( eta_j_kp1
             - eta_j_k
             ) 
           / dx;
      dvv  = -dt 
           * g
           * ( eta_jp1_k
             - eta_j_k
             ) 
           / dy;

      //prediction for u and v (merged loop)
      //---------------------------------
      uu = u_j_k;
      if (  ( (wet_j_k == 1)
              && ( (wet_j_kp1 == 1) || (duu > 0.0)))
         || ( (wet_j_kp1 == 1) && (duu < 0.0))     
         ){
          un_j_k = uu+duu;
      }//if
      
      vv = v_j_k;
      if (  (  (wet_j_k == 1)
             && ( (wet_jp1_k == 1) || (dvv > 0.0)))
         || ((wet_jp1_k == 1) && (dvv < 0.0))     
         ){
          vn_j_k = vv+dvv;
      }//if

      //#ifdef DEBUG_KERNEL
      //printf("K_DYN1:compindex = %d; computed: j = %d, k = %d, un = %f, vn = %f, wet = %f, wet_jp1_k = %f, wet_j_kp1 = %f,  eta_j_k = %f, eta_jp1_k = %f, eta_j_kp1 = %f,uu = %f, duu = %f, vv = %f, dvv = %f \n"
      //             ,compindex,j, k, un_j_k, vn_j_k, wet_j_k, wet_jp1_k, wet_j_kp1, eta_j_k,  eta_jp1_k, eta_j_kp1, uu, duu, vv, dvv);
      //#endif
    }//if not boundary
      
   //   #ifdef DEBUG_KERNEL
   //   printf("K_DYN1-WRITE-ATTEMPT: count = %d, buffindex = %d, compindex = %d; j = %d, k = %d; Attempting to READ to channels\n", count, buffindex, compindex, j, k);
   //   #endif      
      
    write_pipe_block(   un_dyn1_2_dyn2_pre , &un_j_k   ); 
    write_pipe_block(   vn_dyn1_2_dyn2_pre , &vn_j_k   ); 
    write_pipe_block(    h_dyn1_2_dyn2_pre , &h_j_k    ); 
    write_pipe_block(  eta_dyn1_2_dyn2_pre , &eta_j_k  ); 
    write_pipe_block( etan_dyn1_2_dyn2_pre , &etan_j_k ); 
    write_pipe_block(  wet_dyn1_2_dyn2_pre , &wet_j_k  );  
    write_pipe_block(hzero_dyn1_2_dyn2_pre , &hzero_j_k); 
    
    //  #ifdef DEBUG_KERNEL
    //  printf("K_DYN1-WRITE_SUCCESS: count = %d, buffindex = %d, compindex = %d; j = %d, k = %d; Attempting to READ to channels\n", count, buffindex, compindex, j, k);
    //  #endif
 }//for l
}//()

// -------------------------------
// SMACHE_DYN1_2_DYN2
// -------------------------------
//If there is a stencil/offset required on an stream between 2 kernels,
//then we introduce this "SMACH" module. This makes the design cleaner and more modular,
//and also make it easier to incorporate tytra-smache approach later on
__kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))
void kernel_smache_dyn1_2_dyn2 ( 
                                  ) {  
  //common parameters
  //-----------------
  const int arrsize   = DATA_SIZE; //Size of  input array(s)
  
  //TODO: I should be able to derive these parameters from given offsets, but this should
  // be a compile-time constant, otherwise I will needlessly waste processing on it
  const int ker_maxoffpos     = COLS; 
  const int ker_maxoffneg     = COLS;    
  const int nloop             = arrsize + ker_maxoffpos;
  
  const int ker_buffsize  = ker_maxoffpos + ker_maxoffneg + 1;
//  const int ind_j_k       = ker_buffsize - 1 - ker_maxoffneg;
  const int ind_j_k       = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k     = ind_j_k + COLS;  
  const int ind_j_kp1     = ind_j_k + 1;     
  const int ind_jm1_k     = ind_j_k - COLS;  
  const int ind_j_km1     = ind_j_k - 1;
 

 // printf("ker_buffsize %d, ind_j_k %d ,ind_jp1_k %d ,ind_j_kp1 %d ,ind_jm1_k %d ,ind_j_km %d\n"
 //         ,ker_buffsize
 //         ,ind_j_k
 //         ,ind_jp1_k
 //         ,ind_j_kp1
 //         ,ind_jm1_k
 //         ,ind_j_km1
 //         );
  
  //buffers
  //--------
  data_t    un_buffer [ker_buffsize];
  data_t    vn_buffer [ker_buffsize];
  data_t     h_buffer [ker_buffsize];
  data_t   eta_buffer [ker_buffsize];
  data_t  etan_buffer [ker_buffsize];
  data_t   wet_buffer [ker_buffsize];
  data_t hzero_buffer [ker_buffsize];

  //The tuple variables to emit
  data_t     un_j_k;
  data_t   un_jm1_k;
  data_t   un_j_km1;
  data_t     vn_j_k;
  data_t   vn_jm1_k;
  data_t   vn_j_km1;
  data_t      h_j_k;
  data_t    h_jm1_k;
  data_t    h_j_km1;
  data_t    h_j_kp1;
  data_t    h_jp1_k;
  data_t    eta_j_k;
  data_t   etan_j_k;
  data_t    wet_j_k;
  data_t  hzero_j_k;
  
  //loop for the entire array + offset buffer
  for (int count=0; count < nloop ; count++) {  
     int compindex = count - ker_maxoffpos;

    //this unrolled loop implements a SHIFT-RIGHT register for the buffer, which is (should be)
    //more effecient than a naive buffer that is written in a RAM fashion
    #pragma unroll 
     for (int i = 0; i < ker_buffsize-1 ; ++i) {
             un_buffer[i] =    un_buffer[i + 1];
             vn_buffer[i] =    vn_buffer[i + 1];
              h_buffer[i] =     h_buffer[i + 1];
            eta_buffer[i] =   eta_buffer[i + 1];
           etan_buffer[i] =  etan_buffer[i + 1];
            wet_buffer[i] =   wet_buffer[i + 1];
          hzero_buffer[i] = hzero_buffer[i + 1];
      }    
      
    //we read into MS-word of buffers until count reaches limit of input array
    if(count < arrsize) {   
      read_pipe_block(   un_dyn1_2_dyn2_pre, &   un_buffer[ker_buffsize-1]);
      read_pipe_block(   vn_dyn1_2_dyn2_pre, &   vn_buffer[ker_buffsize-1]);
      read_pipe_block(    h_dyn1_2_dyn2_pre, &    h_buffer[ker_buffsize-1]);
      read_pipe_block(  eta_dyn1_2_dyn2_pre, &  eta_buffer[ker_buffsize-1]);
      read_pipe_block( etan_dyn1_2_dyn2_pre, & etan_buffer[ker_buffsize-1]);
      read_pipe_block(  wet_dyn1_2_dyn2_pre, &  wet_buffer[ker_buffsize-1]);
      read_pipe_block(hzero_dyn1_2_dyn2_pre, &hzero_buffer[ker_buffsize-1]);  
    }//if
    
    //start emitting required data/stencil when compindex reaches 0
    if(compindex>=0) {
      //get the data and stencil values from buffer
         un_j_k =    un_buffer[ind_j_k]; 
       un_jm1_k =    un_buffer[ind_jm1_k]; 
       un_j_km1 =    un_buffer[ind_j_km1];
         vn_j_k =    vn_buffer[ind_j_k];
       vn_jm1_k =    vn_buffer[ind_jm1_k];
       vn_j_km1 =    vn_buffer[ind_j_km1];
          h_j_k =     h_buffer[ind_j_k];
        h_jm1_k =     h_buffer[ind_jm1_k];
        h_j_km1 =     h_buffer[ind_j_km1];
        h_j_kp1 =     h_buffer[ind_j_kp1];
        h_jp1_k =     h_buffer[ind_jp1_k];
        eta_j_k =   eta_buffer[ind_j_k];
       etan_j_k =  etan_buffer[ind_j_k];
        wet_j_k =   wet_buffer[ind_j_k];
      hzero_j_k = hzero_buffer[ind_j_k];
  
      write_pipe_block (   un_j_k_dyn1_2_dyn2_post,  &  un_j_k); 
      write_pipe_block ( un_jm1_k_dyn1_2_dyn2_post,  &un_jm1_k); 
      write_pipe_block ( un_j_km1_dyn1_2_dyn2_post,  &un_j_km1); 
      write_pipe_block (   vn_j_k_dyn1_2_dyn2_post,  &  vn_j_k); 
      write_pipe_block ( vn_jm1_k_dyn1_2_dyn2_post,  &vn_jm1_k); 
      write_pipe_block ( vn_j_km1_dyn1_2_dyn2_post,  &vn_j_km1); 
      write_pipe_block (    h_j_k_dyn1_2_dyn2_post,  &   h_j_k); 
      write_pipe_block (  h_jm1_k_dyn1_2_dyn2_post,  & h_jm1_k); 
      write_pipe_block (  h_j_km1_dyn1_2_dyn2_post,  & h_j_km1); 
      write_pipe_block (  h_j_kp1_dyn1_2_dyn2_post,  & h_j_kp1); 
      write_pipe_block (  h_jp1_k_dyn1_2_dyn2_post,  & h_jp1_k); 
      write_pipe_block (  eta_j_k_dyn1_2_dyn2_post,  & eta_j_k); 
      write_pipe_block ( etan_j_k_dyn1_2_dyn2_post,  &etan_j_k); 
      write_pipe_block (  wet_j_k_dyn1_2_dyn2_post,  & wet_j_k); 
      write_pipe_block (hzero_j_k_dyn1_2_dyn2_post,  &hzero_j_k); 
    }//if(compindex>=0)
  }//for     
}

// -------------------------------
// DYN2
// -------------------------------
__kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))
void kernel_dyn2( const data_t dt
                         , const data_t dx
                         , const data_t dy
                         ) {

  //local
  //-----
  const int arrsize   = DATA_SIZE;    
  const int nloop     = arrsize; 

  int compindex; 
  int j, k;      

  data_t hue;
  data_t huw;
  data_t hwp;
  data_t hwn;
  data_t hen;
  data_t hep;
  data_t hvn;
  data_t hvs;
  data_t hsp;
  data_t hsn;
  data_t hnn;
  data_t hnp;

  //input from channels
  data_t    un_j_k;
  data_t  un_jm1_k;
  data_t  un_j_km1;
  data_t    vn_j_k;
  data_t  vn_jm1_k;
  data_t  vn_j_km1;
  data_t     h_j_k;
  data_t   h_jm1_k;
  data_t   h_j_km1;
  data_t   h_j_kp1;
  data_t   h_jp1_k;
  data_t   eta_j_k;
  data_t  etan_j_k;
  data_t   wet_j_k;
  data_t hzero_j_k;  
 
  for (int count=0; count < nloop; count++) {  
    compindex = count;
    j = compindex/COLS;
    k = compindex%COLS;

    //#ifdef DEBUG_KERNEL
    //printf("K_DYN2-LOOPTOP: count_dyn2 = %d, arrsize = %d, nloop = %d, buffindex = %d, compindex = %d; j = %d, k = %d; \n", count_dyn2, arrsize, nloop, buffindex, compindex, j, k);
    //#endif
    read_pipe_block (   un_j_k_dyn1_2_dyn2_post, &  un_j_k );
    read_pipe_block ( un_jm1_k_dyn1_2_dyn2_post, &un_jm1_k );
    read_pipe_block ( un_j_km1_dyn1_2_dyn2_post, &un_j_km1 );
    read_pipe_block (   vn_j_k_dyn1_2_dyn2_post, &  vn_j_k );
    read_pipe_block ( vn_jm1_k_dyn1_2_dyn2_post, &vn_jm1_k );
    read_pipe_block ( vn_j_km1_dyn1_2_dyn2_post, &vn_j_km1 );
    read_pipe_block (    h_j_k_dyn1_2_dyn2_post, &   h_j_k );
    read_pipe_block (  h_jm1_k_dyn1_2_dyn2_post, & h_jm1_k );
    read_pipe_block (  h_j_km1_dyn1_2_dyn2_post, & h_j_km1 );
    read_pipe_block (  h_j_kp1_dyn1_2_dyn2_post, & h_j_kp1 );
    read_pipe_block (  h_jp1_k_dyn1_2_dyn2_post, & h_jp1_k );
    read_pipe_block (  eta_j_k_dyn1_2_dyn2_post, & eta_j_k );
    read_pipe_block ( etan_j_k_dyn1_2_dyn2_post, &etan_j_k );
    read_pipe_block (  wet_j_k_dyn1_2_dyn2_post, & wet_j_k );
    read_pipe_block (hzero_j_k_dyn1_2_dyn2_post, &hzero_j_k); 
    
    //exclude boundaries
    if  ((j>=1) && (k>=1) && (j<= ROWS-2) && (k<=COLS-2)) {      
      hep = 0.5*( un_j_k + ABS(un_j_k) ) * h_j_k;
      hen = 0.5*( un_j_k - ABS(un_j_k) ) * h_j_kp1;
      hue = hep+hen;
  
      hwp = 0.5*( un_j_km1 + ABS(un_j_km1) ) * h_j_km1;
      hwn = 0.5*( un_j_km1 - ABS(un_j_km1) ) * h_j_k;
      huw = hwp+hwn;
  
      hnp = 0.5*( vn_j_k + ABS(vn_j_k) ) * h_j_k;
      hnn = 0.5*( vn_j_k - ABS(vn_j_k) ) * h_jp1_k;
      hvn = hnp+hnn;
  
      hsp = 0.5*( vn_jm1_k + ABS(vn_jm1_k) ) * h_jm1_k;
      hsn = 0.5*( vn_jm1_k - ABS(vn_jm1_k) ) * h_j_k;
      hvs = hsp+hsn;
  
      etan_j_k  = eta_j_k
                - dt*(hue-huw)/dx
                - dt*(hvn-hvs)/dy;
                
      //#ifdef DEBUG_KERNEL
      //printf("K_DYN2-COMPUTE:count = %d, compindex = %d; computed: j = %d, k = %d, etan_j_k = %f , eta_j_k = %f \n", count_dyn2, compindex, j, k,  etan_j_k, eta_j_k);
      //#endif
    }//if not boundary

    //#ifdef DEBUG_KERNEL
    //printf("K_DYN2-WRITE-ATTEMPT:count_dyn2 = %d, compindex = %d; j = %d, k = %d; Attempting to write to channels\n", count_dyn2, compindex, j, k);
    //#endif
    
    write_pipe_block(   un_dyn2_2_shapiro_pre , &un_j_k   );
    write_pipe_block(   vn_dyn2_2_shapiro_pre , &vn_j_k   );
    write_pipe_block(  eta_dyn2_2_shapiro_pre , &eta_j_k  );
    write_pipe_block( etan_dyn2_2_shapiro_pre , &etan_j_k );
    write_pipe_block(  wet_dyn2_2_shapiro_pre , &wet_j_k  ); 
    write_pipe_block(hzero_dyn2_2_shapiro_pre , &hzero_j_k); 
    
    
    //#ifdef DEBUG_KERNEL
    //printf("K_DYN2-ENDOFLOOP: nloop = %d, count_dyn2 = %d, compindex = %d; j = %d, k = %d; \n", nloop, count_dyn2, compindex, j, k);
    //printf("----------------------------------------------------------------------------\n");
    //#endif
  }//for
}//()

// -------------------------------
// SMACHE_DYN2_2_SHAPIRO
// -------------------------------
//If there is a stencil/offset required on an stream between 2 kernels,
//then we introduce this "SMACH" module. This makes the design cleaner and more modular,
//and also make it easier to incorporate tytra-smache approach later on
__kernel void __attribute__ ((reqd_work_group_size(1, 1, 1)))
kernel_smache_dyn2_2_shapiro ( 
                                  ) {  
  //common parameters
  //-----------------
  const int arrsize   = DATA_SIZE; //Size of  input array(s)
  
  //TODO: I should be able to derive these parameters from given offsets, but this should
  // be a compile-time constant, otherwise I will needlessly waste processing on it
  const int ker_maxoffpos     = COLS; 
  const int ker_maxoffneg     = COLS;    
  const int nloop             = arrsize + ker_maxoffpos;
  
  const int ker_buffsize  = ker_maxoffpos + ker_maxoffneg + 1;
  //const int ind_j_k       = ker_buffsize - 1 - ker_maxoffneg;
  const int ind_j_k       = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k     = ind_j_k + COLS;  
  const int ind_j_kp1     = ind_j_k + 1;     
  const int ind_jm1_k     = ind_j_k - COLS;  
  const int ind_j_km1     = ind_j_k - 1;     
  
  //buffers
  //--------
  data_t    un_buffer [ker_buffsize];
  data_t    vn_buffer [ker_buffsize];
  data_t   eta_buffer [ker_buffsize];
  data_t  etan_buffer [ker_buffsize];
  data_t   wet_buffer [ker_buffsize];
  data_t hzero_buffer [ker_buffsize];

  //The tuple variables to emit
  data_t     un_j_k;
  data_t     vn_j_k;
  data_t      h_j_k;
  data_t    eta_j_k;
  data_t   etan_j_k;
  data_t etan_jm1_k;
  data_t etan_j_km1;
  data_t etan_j_kp1;
  data_t etan_jp1_k;
  data_t    wet_j_k;
  data_t  wet_jm1_k;
  data_t  wet_j_km1;
  data_t  wet_j_kp1;
  data_t  wet_jp1_k;
  data_t  hzero_j_k;
  
  //loop for the entire array + offset buffer
  for (int count=0; count < nloop ; count++) {  
     int compindex = count - ker_maxoffpos;

    //this unrolled loop implements a SHIFT-RIGHT register for the buffer, which is (should be)
    //more effecient than a naive buffer that is written in a RAM fashion
    #pragma unroll 
     for (int i = 0; i < ker_buffsize-1 ; ++i) {
             un_buffer[i] =    un_buffer[i + 1];
             vn_buffer[i] =    vn_buffer[i + 1];
            eta_buffer[i] =   eta_buffer[i + 1];
           etan_buffer[i] =  etan_buffer[i + 1];
            wet_buffer[i] =   wet_buffer[i + 1];
          hzero_buffer[i] = hzero_buffer[i + 1];
      }    
      
    //we read into MS-word of buffers until count reaches limit of input array
    if(count < arrsize) {   
      read_pipe_block(   un_dyn2_2_shapiro_pre, &   un_buffer[ker_buffsize-1]); 
      read_pipe_block(   vn_dyn2_2_shapiro_pre, &   vn_buffer[ker_buffsize-1]); 
      read_pipe_block(  eta_dyn2_2_shapiro_pre, &  eta_buffer[ker_buffsize-1]); 
      read_pipe_block( etan_dyn2_2_shapiro_pre, & etan_buffer[ker_buffsize-1]); 
      read_pipe_block(  wet_dyn2_2_shapiro_pre, &  wet_buffer[ker_buffsize-1]); 
      read_pipe_block(hzero_dyn2_2_shapiro_pre, &hzero_buffer[ker_buffsize-1]);  
    }//if
    
    //start emitting required data/stencil when compindex reaches 0
    if(compindex>=0) {
      //get the data and stencil values from buffer
         un_j_k =    un_buffer[ind_j_k]; 
         vn_j_k =    vn_buffer[ind_j_k];
        eta_j_k =   eta_buffer[ind_j_k];
       etan_j_k =  etan_buffer[ind_j_k];
     etan_jm1_k =  etan_buffer[ind_jm1_k];
     etan_j_km1 =  etan_buffer[ind_j_km1];
     etan_j_kp1 =  etan_buffer[ind_j_kp1];
     etan_jp1_k =  etan_buffer[ind_jp1_k];
        wet_j_k =   wet_buffer[ind_j_k];
      wet_jm1_k =   wet_buffer[ind_jm1_k];
      wet_j_km1 =   wet_buffer[ind_j_km1];
      wet_j_kp1 =   wet_buffer[ind_j_kp1];
      wet_jp1_k =   wet_buffer[ind_jp1_k];
      hzero_j_k = hzero_buffer[ind_j_k];
  
      write_pipe_block (    un_j_k_dyn2_2_shapiro_post, &    un_j_k); 
      write_pipe_block (    vn_j_k_dyn2_2_shapiro_post, &    vn_j_k); 
      write_pipe_block (   eta_j_k_dyn2_2_shapiro_post, &   eta_j_k); 
      write_pipe_block (  etan_j_k_dyn2_2_shapiro_post, &  etan_j_k); 
      write_pipe_block (etan_jm1_k_dyn2_2_shapiro_post, &etan_jm1_k); 
      write_pipe_block (etan_j_km1_dyn2_2_shapiro_post, &etan_j_km1); 
      write_pipe_block (etan_j_kp1_dyn2_2_shapiro_post, &etan_j_kp1); 
      write_pipe_block (etan_jp1_k_dyn2_2_shapiro_post, &etan_jp1_k); 
      write_pipe_block (   wet_j_k_dyn2_2_shapiro_post, &   wet_j_k); 
      write_pipe_block ( wet_jm1_k_dyn2_2_shapiro_post, & wet_jm1_k); 
      write_pipe_block ( wet_j_km1_dyn2_2_shapiro_post, & wet_j_km1); 
      write_pipe_block ( wet_j_kp1_dyn2_2_shapiro_post, & wet_j_kp1); 
      write_pipe_block ( wet_jp1_k_dyn2_2_shapiro_post, & wet_jp1_k); 
      write_pipe_block ( hzero_j_k_dyn2_2_shapiro_post, & hzero_j_k); 
    }//if(compindex>=0)
  }//for     
}



//------------------------------------------
// SHAPIRO KERNEL
//------------------------------------------
__kernel void __attribute__ ((reqd_work_group_size(1, 1, 1)))
kernel_shapiro  ( const data_t eps 
                              ) {

  const int arrsize   = DATA_SIZE;   
  const int nloop     = arrsize;

  int compindex; 
                 
  int j, k;      

  //locals
  data_t term1,term2,term3;

  
  //input from channels
  data_t     un_j_k;
  data_t     vn_j_k;
  data_t    eta_j_k;
  data_t   etan_j_k;
  data_t etan_jm1_k;
  data_t etan_j_km1;
  data_t etan_j_kp1;
  data_t etan_jp1_k;
  data_t    wet_j_k;
  data_t  wet_jm1_k;
  data_t  wet_j_km1;
  data_t  wet_j_kp1;
  data_t  wet_jp1_k;
  data_t  hzero_j_k;
 
  
  // The main loop //
  for (int count=0; count < nloop; count++) {  
    compindex = count;
    j = compindex/COLS;
    k = compindex%COLS;
      
    read_pipe_block (    un_j_k_dyn2_2_shapiro_post, &    un_j_k);
    read_pipe_block (    vn_j_k_dyn2_2_shapiro_post, &    vn_j_k);
    read_pipe_block (   eta_j_k_dyn2_2_shapiro_post, &   eta_j_k);
    read_pipe_block (  etan_j_k_dyn2_2_shapiro_post, &  etan_j_k);
    read_pipe_block (etan_jm1_k_dyn2_2_shapiro_post, &etan_jm1_k);
    read_pipe_block (etan_j_km1_dyn2_2_shapiro_post, &etan_j_km1);
    read_pipe_block (etan_j_kp1_dyn2_2_shapiro_post, &etan_j_kp1);
    read_pipe_block (etan_jp1_k_dyn2_2_shapiro_post, &etan_jp1_k);
    read_pipe_block (   wet_j_k_dyn2_2_shapiro_post, &   wet_j_k);
    read_pipe_block ( wet_jm1_k_dyn2_2_shapiro_post, & wet_jm1_k);
    read_pipe_block ( wet_j_km1_dyn2_2_shapiro_post, & wet_j_km1);
    read_pipe_block ( wet_j_kp1_dyn2_2_shapiro_post, & wet_j_kp1);
    read_pipe_block ( wet_jp1_k_dyn2_2_shapiro_post, & wet_jp1_k);
    read_pipe_block ( hzero_j_k_dyn2_2_shapiro_post, & hzero_j_k); 
      
      //exclude boundaries
      if  ((j>=1) && (k>=1) && (j<= ROWS-2) && (k<=COLS-2)) {      
          if (wet_j_k==1) {
          term1 = ( 1.0-0.25*eps
                    * ( wet_j_kp1
                      + wet_j_km1
                      + wet_jp1_k
                      + wet_jm1_k
                      ) 
                  )
                  * etan_j_k;
          term2 = 0.25*eps
                  * ( wet_j_kp1
                    * etan_j_kp1
                    + wet_j_km1
                    * etan_j_km1
                    );
          term3 = 0.25*eps
                  * ( wet_jp1_k
                    * etan_jp1_k
                    + wet_jm1_k
                    * etan_jm1_k
                    );
          eta_j_k = term1 + term2 + term3;
        }//if
        else {
          eta_j_k = etan_j_k;
        }//else

//      #ifdef DEBUG_KERNEL
//      printf("K_SHAPIRO\t:compindex = %d; computed: j = %d, k = %d, eta = %f, wet_j_k = %f, wet_jp1_k = %f, wet_j_kp1 = %f, wet_jm1_k = %f, wet_j_km1 = %f \n"
//                      ,compindex, j, k, eta_j_k
//                      ,wet_j_k                      
//                      ,wet_jp1_k 
//                      ,wet_j_kp1 
//                      ,wet_jm1_k 
//                      ,wet_j_km1 
//      );
//      #endif

      }//if not boundary
    
      write_pipe_block(eta_shapiro_2_udpate   , &eta_j_k);   
      write_pipe_block(un_shapiro_2_udpate    , &un_j_k);    
      write_pipe_block(vn_shapiro_2_udpate    , &vn_j_k);    
      write_pipe_block(hzero_shapiro_2_udpate , &hzero_j_k); 
 }//for l
}//()


//------------------------------------------
// UPDATES KERNEL
//------------------------------------------
__kernel void __attribute__ ((reqd_work_group_size(1, 1, 1)))
kernel_updates  ( data_t hmin 
                ) {
  int j, k, index;
  data_t h_j_k;
  data_t u_j_k;
  data_t v_j_k;
  data_t hzero_j_k ;
  data_t eta_j_k   ;
  data_t un_j_k    ;
  data_t vn_j_k    ;
  data_t wet_j_k   ;

  //we need to iterate over the entire space inc boundaries as we are reading from pipe and not RAM
  for (int index=0; index < DATA_SIZE; index++) {     
      read_pipe_block(eta_shapiro_2_udpate  , &eta_j_k  );     
      read_pipe_block(un_shapiro_2_udpate   , &un_j_k   );     
      read_pipe_block(vn_shapiro_2_udpate   , &vn_j_k   );     
      read_pipe_block(hzero_shapiro_2_udpate, &hzero_j_k); 

      int j = index/COLS;
      int k = index%COLS;

      //h update
      h_j_k = hzero_j_k
            + eta_j_k;
      //wet update
      wet_j_k = 1;
      if ( h_j_k < hmin )
            wet_j_k = 0;
      //u, v updates
      u_j_k = un_j_k;
      v_j_k = vn_j_k;

 //     #ifdef DEBUG_KERNEL
 //     printf("K_UPDATE\t:index = %d; computed: j = %d, k = %d, h = %f, h_zero = %f, eta_j_k = %f \n"
 //                     ,index, j, k
 //                     ,h_j_k
 //                     ,hzero_j_k
 //                     ,eta_j_k
 //     );
 //     #endif

      write_pipe_block(u_out_update   , &u_j_k);  
      write_pipe_block(v_out_update   , &v_j_k);   
      write_pipe_block(h_out_update   , &h_j_k);   
      write_pipe_block(eta_out_update , &eta_j_k);
      write_pipe_block(wet_out_update , &wet_j_k); 
  }//for
}//()


//------------------------------------------
// Write memory kernel
//------------------------------------------
kernel void kernel_mem_wr  (__global data_t* restrict u
                           ,__global data_t* restrict v
                           ,__global data_t* restrict h
                           ,__global data_t* restrict eta
                           ,__global data_t* restrict wet
) {
  for (int index=0; index < DATA_SIZE; index++) {       
      data_t u_new   ;  
      data_t v_new   ;  
      data_t h_new   ;  
      data_t eta_new ;
      data_t wet_new ;
      
      read_pipe_block(u_out_update   , &u_new   ); 
      read_pipe_block(v_out_update   , &v_new   ); 
      read_pipe_block(h_out_update   , &h_new   ); 
      read_pipe_block(eta_out_update , &eta_new ); 
      read_pipe_block(wet_out_update , &wet_new ); 
      
      u[index] = u_new;
      v[index] = v_new;
      h[index] = h_new;
      eta[index] = eta_new;
      wet[index] = wet_new;
  }
}//()

