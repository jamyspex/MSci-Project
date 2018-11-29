// Waqar Nabi, Dec 5 2016
//

//#define DEBUG_KERNEL

// include the custom header file generated for this run
#include "kernelCompilerInclude.h"
// -------------------------------
// Dealing with TYPES
// -------------------------------
//needed if we want to work with double
#if WORD==DOUBLE
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#endif

//If we are using floats or doubles, we use floating version of abs (fabs)
#if WORD==INT
 #define ABS abs
#else
 #define ABS fabs
#endif
 
// -------------------------------
// AOCL specific
// -------------------------------
#pragma OPENCL EXTENSION cl_altera_channels : enable


#if TARGET==AOCL
#if NUM_COMPUTE_UNITS>1 
  __attribute__((num_compute_units(NUM_COMPUTE_UNITS)))
#endif
  
#if NUM_SIMD_ITEMS>1
  __attribute__((num_simd_work_items(NUM_SIMD_ITEMS)))
#endif
  
  //#ifdef REQ_WORKGROUP_SIZE
  //  __attribute__((reqd_work_group_size(REQ_WORKGROUP_SIZE)))
  //#endif
#endif

// -------------------------------
// SDACCEL specific
// -------------------------------
#if TARGET==SDACCEL
#endif    

// -------------------------------
// GENERIC attributes/opimizations
// -------------------------------
#ifdef REQ_WORKGROUPIZE
    __attribute__((reqd_work_group_size(REQ_WORKGROUP_SIZE)))
#endif

// -------------------------------
// Channel declarations
// -------------------------------
//if stencil is required on any stream between two kernels, then
//we insert a "smache" module in-between, which requires two versions of each
//channel... pre and post 
//the post channel will also contain any additional channels for stencil elements


//MEMRD-2-DYN1
channel stypeDevice     u_memrd_2_dyn1_pre;
channel stypeDevice     v_memrd_2_dyn1_pre;
channel stypeDevice     h_memrd_2_dyn1_pre;
channel stypeDevice   eta_memrd_2_dyn1_pre;
channel stypeDevice  etan_memrd_2_dyn1_pre;
channel stypeDevice   wet_memrd_2_dyn1_pre;
channel stypeDevice hzero_memrd_2_dyn1_pre;

channel stypeDevice     u_j_k_memrd_2_dyn1_post;
channel stypeDevice     v_j_k_memrd_2_dyn1_post;
channel stypeDevice     h_j_k_memrd_2_dyn1_post;
channel stypeDevice   eta_j_k_memrd_2_dyn1_post;
channel stypeDevice eta_j_kp1_memrd_2_dyn1_post;
channel stypeDevice eta_jp1_k_memrd_2_dyn1_post;
channel stypeDevice  etan_j_k_memrd_2_dyn1_post;
channel stypeDevice   wet_j_k_memrd_2_dyn1_post;
channel stypeDevice wet_j_kp1_memrd_2_dyn1_post;
channel stypeDevice wet_jp1_k_memrd_2_dyn1_post;
channel stypeDevice hzero_j_k_memrd_2_dyn1_post;

//DYN1-2-DYN2
channel stypeDevice    un_dyn1_2_dyn2_pre;
channel stypeDevice    vn_dyn1_2_dyn2_pre;
channel stypeDevice     h_dyn1_2_dyn2_pre;
channel stypeDevice   eta_dyn1_2_dyn2_pre;
channel stypeDevice  etan_dyn1_2_dyn2_pre;
channel stypeDevice   wet_dyn1_2_dyn2_pre;
channel stypeDevice hzero_dyn1_2_dyn2_pre;

channel stypeDevice    un_j_k_dyn1_2_dyn2_post;
channel stypeDevice  un_jm1_k_dyn1_2_dyn2_post;
channel stypeDevice  un_j_km1_dyn1_2_dyn2_post;
channel stypeDevice    vn_j_k_dyn1_2_dyn2_post;
channel stypeDevice  vn_jm1_k_dyn1_2_dyn2_post;
channel stypeDevice  vn_j_km1_dyn1_2_dyn2_post;
channel stypeDevice     h_j_k_dyn1_2_dyn2_post;
channel stypeDevice   h_jm1_k_dyn1_2_dyn2_post;
channel stypeDevice   h_j_km1_dyn1_2_dyn2_post;
channel stypeDevice   h_j_kp1_dyn1_2_dyn2_post;
channel stypeDevice   h_jp1_k_dyn1_2_dyn2_post;
channel stypeDevice   eta_j_k_dyn1_2_dyn2_post;
channel stypeDevice  etan_j_k_dyn1_2_dyn2_post;
channel stypeDevice   wet_j_k_dyn1_2_dyn2_post;
channel stypeDevice hzero_j_k_dyn1_2_dyn2_post;

//DYN2-2-SHAPIRO
channel stypeDevice    un_dyn2_2_shapiro_pre ;
channel stypeDevice    vn_dyn2_2_shapiro_pre ;
channel stypeDevice   eta_dyn2_2_shapiro_pre ;
channel stypeDevice  etan_dyn2_2_shapiro_pre ;
channel stypeDevice   wet_dyn2_2_shapiro_pre ;
channel stypeDevice hzero_dyn2_2_shapiro_pre ;

channel stypeDevice     un_j_k_dyn2_2_shapiro_post;
channel stypeDevice     vn_j_k_dyn2_2_shapiro_post;
channel stypeDevice    eta_j_k_dyn2_2_shapiro_post;
channel stypeDevice   etan_j_k_dyn2_2_shapiro_post;
channel stypeDevice etan_jm1_k_dyn2_2_shapiro_post;
channel stypeDevice etan_j_km1_dyn2_2_shapiro_post;
channel stypeDevice etan_j_kp1_dyn2_2_shapiro_post;
channel stypeDevice etan_jp1_k_dyn2_2_shapiro_post;
channel stypeDevice    wet_j_k_dyn2_2_shapiro_post;
channel stypeDevice  wet_jm1_k_dyn2_2_shapiro_post;
channel stypeDevice  wet_j_km1_dyn2_2_shapiro_post;
channel stypeDevice  wet_j_kp1_dyn2_2_shapiro_post;
channel stypeDevice  wet_jp1_k_dyn2_2_shapiro_post;
channel stypeDevice  hzero_j_k_dyn2_2_shapiro_post;

//SHAPIRO-2-UPDATE
channel stypeDevice eta_shapiro_2_udpate    ;//__attribute__((depth(2)));
channel stypeDevice un_shapiro_2_udpate     ;//__attribute__((depth(2)));
channel stypeDevice vn_shapiro_2_udpate     ;//__attribute__((depth(2)));
channel stypeDevice h_shapiro_2_udpate      ;//__attribute__((depth(2)));
channel stypeDevice hzero_shapiro_2_udpate  ;//__attribute__((depth(2)));

//UPDATE-2-MEMWR
channel stypeDevice  u_out_update     ;//__attribute__((depth(2)));
channel stypeDevice  v_out_update     ;//__attribute__((depth(2)));
channel stypeDevice  h_out_update     ;//__attribute__((depth(2)));
channel stypeDevice  eta_out_update   ;//__attribute__((depth(2)));
channel stypeDevice  wet_out_update   ;//__attribute__((depth(2)));


// ===============================
// KERNELS
// ===============================

//------------------------------------------
// Read memory kernel
//------------------------------------------

__kernel void kernel_mem_rd( __global stypeDevice * restrict u
                       , __global stypeDevice * restrict v
                       , __global stypeDevice * restrict h
                       , __global stypeDevice * restrict eta
                       , __global stypeDevice * restrict etan
                       , __global stypeDevice * restrict wet
                       , __global stypeDevice * restrict hzero
                      ) {
  uint j, k, index;

//  for (index=0; index < (SIZE + COLS); index++) {     
  for (index=0; index < SIZE; index++) {     
      stypeDevice u_data = u[index];
      stypeDevice v_data = v[index];
      stypeDevice h_data = h[index];
      stypeDevice eta_data  = eta[index];
      stypeDevice etan_data = etan[index];
      stypeDevice wet_data  = wet[index];
      stypeDevice hzero_data= hzero[index];

      //#ifdef DEBUG_KERNEL
      //printf("K_MEM-RD-ATTEMPT-WRITE: index = %d\n", index);
      //#endif

      //the write and reads from the channel must be in the same sequence, synched with memory fences
      //otherwise a deadlock can occur as the channels can be read/written out of order...
      write_channel_altera(u_memrd_2_dyn1_pre     ,u_data   );    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(v_memrd_2_dyn1_pre     ,v_data   );    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(h_memrd_2_dyn1_pre     ,h_data   );    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(eta_memrd_2_dyn1_pre   ,eta_data );    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(etan_memrd_2_dyn1_pre  ,etan_data );   mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(wet_memrd_2_dyn1_pre   ,wet_data );    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(hzero_memrd_2_dyn1_pre ,hzero_data );  

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
__kernel void kernel_smache_memrd_2_dyn1 (){   
  //common parameters
  //-----------------
  const int arrsize   = SIZE; //Size of  input array(s)
  
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
  stypeDevice     u_buffer [ker_buffsize];
  stypeDevice     v_buffer [ker_buffsize];
  stypeDevice     h_buffer [ker_buffsize];
  stypeDevice   eta_buffer [ker_buffsize];
  stypeDevice  etan_buffer [ker_buffsize];
  stypeDevice   wet_buffer [ker_buffsize];
  stypeDevice hzero_buffer [ker_buffsize];

  //The tuple variables to emit
  stypeDevice     u_j_k;
  stypeDevice     v_j_k;
  stypeDevice     h_j_k;
  stypeDevice   eta_j_k;
  stypeDevice eta_j_kp1;
  stypeDevice eta_jp1_k;
  stypeDevice  etan_j_k;
  stypeDevice   wet_j_k;
  stypeDevice wet_j_kp1;
  stypeDevice wet_jp1_k;
  stypeDevice hzero_j_k;
  
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
         u_buffer[ker_buffsize-1] = read_channel_altera(u_memrd_2_dyn1_pre     );  mem_fence(CLK_CHANNEL_MEM_FENCE);
         v_buffer[ker_buffsize-1] = read_channel_altera(v_memrd_2_dyn1_pre     );  mem_fence(CLK_CHANNEL_MEM_FENCE);
         h_buffer[ker_buffsize-1] = read_channel_altera(h_memrd_2_dyn1_pre     );  mem_fence(CLK_CHANNEL_MEM_FENCE);
       eta_buffer[ker_buffsize-1] = read_channel_altera(eta_memrd_2_dyn1_pre   );  mem_fence(CLK_CHANNEL_MEM_FENCE);
      etan_buffer[ker_buffsize-1] = read_channel_altera(etan_memrd_2_dyn1_pre  );  mem_fence(CLK_CHANNEL_MEM_FENCE);
       wet_buffer[ker_buffsize-1] = read_channel_altera(wet_memrd_2_dyn1_pre   );  mem_fence(CLK_CHANNEL_MEM_FENCE);
     hzero_buffer[ker_buffsize-1] = read_channel_altera(hzero_memrd_2_dyn1_pre );  
      
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

  
      write_channel_altera (    u_j_k_memrd_2_dyn1_post,     u_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (    v_j_k_memrd_2_dyn1_post,     v_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (    h_j_k_memrd_2_dyn1_post,     h_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  eta_j_k_memrd_2_dyn1_post,   eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (eta_j_kp1_memrd_2_dyn1_post, eta_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (eta_jp1_k_memrd_2_dyn1_post, eta_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( etan_j_k_memrd_2_dyn1_post,  etan_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  wet_j_k_memrd_2_dyn1_post,   wet_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (wet_j_kp1_memrd_2_dyn1_post, wet_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (wet_jp1_k_memrd_2_dyn1_post, wet_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (hzero_j_k_memrd_2_dyn1_post, hzero_j_k); 
    }//if(compindex>=0)
  }//for     
}//()

// -------------------------------
// DYN1
// -------------------------------
__kernel void kernel_dyn1( const stypeDevice dt
                         , const stypeDevice dx
                         , const stypeDevice dy
                         , const stypeDevice g
                         ) {

  //locals
  //-------------------
  stypeDevice du;
  stypeDevice dv;
  stypeDevice uu;
  stypeDevice vv;
  stypeDevice duu;
  stypeDevice dvv;

  const int arrsize   = SIZE; //Size of  input array(s)
  const int nloop     = arrsize;

  int compindex; 
  int j, k;      

  //input from channels
  //-------------------
  stypeDevice u_j_k     ;
  stypeDevice v_j_k     ;
  stypeDevice h_j_k     ;
  stypeDevice eta_j_k   ;
  stypeDevice eta_j_kp1 ;
  stypeDevice eta_jp1_k ;
  stypeDevice etan_j_k  ;
  stypeDevice wet_j_k   ;
  stypeDevice wet_j_kp1 ;
  stypeDevice wet_jp1_k ;
  stypeDevice hzero_j_k ;


  //loop for the entire array
  for (int count=0; count < nloop ; count++) {  
    compindex = count;
    j = compindex/COLS;
    k = compindex%COLS;

    //#ifdef DEBUG_KERNEL
    //printf("kernel_dyn1-READ-A\t: count = %d\n", count);
    //#endif

    u_j_k     = read_channel_altera(    u_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    v_j_k     = read_channel_altera(    v_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    h_j_k     = read_channel_altera(    h_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    eta_j_k   = read_channel_altera(  eta_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    eta_j_kp1 = read_channel_altera(eta_j_kp1_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    eta_jp1_k = read_channel_altera(eta_jp1_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_k  = read_channel_altera( etan_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    wet_j_k   = read_channel_altera(  wet_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    wet_j_kp1 = read_channel_altera(wet_j_kp1_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    wet_jp1_k = read_channel_altera(wet_jp1_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    hzero_j_k = read_channel_altera(hzero_j_k_memrd_2_dyn1_post); 

    //#ifdef DEBUG_KERNEL
    //printf("kernel_dyn1-READ-B\t: count = %d\n", count);
    //#endif
    
    stypeDevice un_j_k = 0.0;
    stypeDevice vn_j_k = 0.0;
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
      
    write_channel_altera(   un_dyn1_2_dyn2_pre ,un_j_k   ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(   vn_dyn1_2_dyn2_pre ,vn_j_k   ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(    h_dyn1_2_dyn2_pre ,h_j_k    ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(  eta_dyn1_2_dyn2_pre ,eta_j_k  ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( etan_dyn1_2_dyn2_pre ,etan_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(  wet_dyn1_2_dyn2_pre ,wet_j_k  ); mem_fence(CLK_CHANNEL_MEM_FENCE); 
    write_channel_altera(hzero_dyn1_2_dyn2_pre ,hzero_j_k); 
    
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
__kernel void kernel_smache_dyn1_2_dyn2 ( 
                                  ) {  
  //common parameters
  //-----------------
  const int arrsize   = SIZE; //Size of  input array(s)
  
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
  stypeDevice    un_buffer [ker_buffsize];
  stypeDevice    vn_buffer [ker_buffsize];
  stypeDevice     h_buffer [ker_buffsize];
  stypeDevice   eta_buffer [ker_buffsize];
  stypeDevice  etan_buffer [ker_buffsize];
  stypeDevice   wet_buffer [ker_buffsize];
  stypeDevice hzero_buffer [ker_buffsize];

  //The tuple variables to emit
  stypeDevice     un_j_k;
  stypeDevice   un_jm1_k;
  stypeDevice   un_j_km1;
  stypeDevice     vn_j_k;
  stypeDevice   vn_jm1_k;
  stypeDevice   vn_j_km1;
  stypeDevice      h_j_k;
  stypeDevice    h_jm1_k;
  stypeDevice    h_j_km1;
  stypeDevice    h_j_kp1;
  stypeDevice    h_jp1_k;
  stypeDevice    eta_j_k;
  stypeDevice   etan_j_k;
  stypeDevice    wet_j_k;
  stypeDevice  hzero_j_k;
  
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
         un_buffer[ker_buffsize-1] = read_channel_altera(   un_dyn1_2_dyn2_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
         vn_buffer[ker_buffsize-1] = read_channel_altera(   vn_dyn1_2_dyn2_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
          h_buffer[ker_buffsize-1] = read_channel_altera(    h_dyn1_2_dyn2_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
        eta_buffer[ker_buffsize-1] = read_channel_altera(  eta_dyn1_2_dyn2_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
       etan_buffer[ker_buffsize-1] = read_channel_altera( etan_dyn1_2_dyn2_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
        wet_buffer[ker_buffsize-1] = read_channel_altera(  wet_dyn1_2_dyn2_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
      hzero_buffer[ker_buffsize-1] = read_channel_altera(hzero_dyn1_2_dyn2_pre);  
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
  
      write_channel_altera (   un_j_k_dyn1_2_dyn2_post,    un_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( un_jm1_k_dyn1_2_dyn2_post,  un_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( un_j_km1_dyn1_2_dyn2_post,  un_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (   vn_j_k_dyn1_2_dyn2_post,    vn_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( vn_jm1_k_dyn1_2_dyn2_post,  vn_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( vn_j_km1_dyn1_2_dyn2_post,  vn_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (    h_j_k_dyn1_2_dyn2_post,     h_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  h_jm1_k_dyn1_2_dyn2_post,   h_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  h_j_km1_dyn1_2_dyn2_post,   h_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  h_j_kp1_dyn1_2_dyn2_post,   h_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  h_jp1_k_dyn1_2_dyn2_post,   h_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  eta_j_k_dyn1_2_dyn2_post,   eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( etan_j_k_dyn1_2_dyn2_post,  etan_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  wet_j_k_dyn1_2_dyn2_post,   wet_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (hzero_j_k_dyn1_2_dyn2_post, hzero_j_k); 
    }//if(compindex>=0)
  }//for     
}

// -------------------------------
// DYN2
// -------------------------------
__kernel void kernel_dyn2( const stypeDevice dt
                         , const stypeDevice dx
                         , const stypeDevice dy
                         ) {

  //local
  //-----
  const int arrsize   = SIZE;    
  const int nloop     = arrsize; 

  int compindex; 
  int j, k;      

  stypeDevice hue;
  stypeDevice huw;
  stypeDevice hwp;
  stypeDevice hwn;
  stypeDevice hen;
  stypeDevice hep;
  stypeDevice hvn;
  stypeDevice hvs;
  stypeDevice hsp;
  stypeDevice hsn;
  stypeDevice hnn;
  stypeDevice hnp;

  //input from channels
  stypeDevice    un_j_k;
  stypeDevice  un_jm1_k;
  stypeDevice  un_j_km1;
  stypeDevice    vn_j_k;
  stypeDevice  vn_jm1_k;
  stypeDevice  vn_j_km1;
  stypeDevice     h_j_k;
  stypeDevice   h_jm1_k;
  stypeDevice   h_j_km1;
  stypeDevice   h_j_kp1;
  stypeDevice   h_jp1_k;
  stypeDevice   eta_j_k;
  stypeDevice  etan_j_k;
  stypeDevice   wet_j_k;
  stypeDevice hzero_j_k;  
 
  for (int count=0; count < nloop; count++) {  
    compindex = count;
    j = compindex/COLS;
    k = compindex%COLS;

    //#ifdef DEBUG_KERNEL
    //printf("K_DYN2-LOOPTOP: count_dyn2 = %d, arrsize = %d, nloop = %d, buffindex = %d, compindex = %d; j = %d, k = %d; \n", count_dyn2, arrsize, nloop, buffindex, compindex, j, k);
    //#endif
      un_j_k  = read_channel_altera (   un_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    un_jm1_k  = read_channel_altera ( un_jm1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    un_j_km1  = read_channel_altera ( un_j_km1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
      vn_j_k  = read_channel_altera (   vn_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    vn_jm1_k  = read_channel_altera ( vn_jm1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    vn_j_km1  = read_channel_altera ( vn_j_km1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
       h_j_k  = read_channel_altera (    h_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_jm1_k  = read_channel_altera (  h_jm1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_j_km1  = read_channel_altera (  h_j_km1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_j_kp1  = read_channel_altera (  h_j_kp1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_jp1_k  = read_channel_altera (  h_jp1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     eta_j_k  = read_channel_altera (  eta_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_k  = read_channel_altera ( etan_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_j_k  = read_channel_altera (  wet_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    hzero_j_k = read_channel_altera (hzero_j_k_dyn1_2_dyn2_post); 
    
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
    
    write_channel_altera(   un_dyn2_2_shapiro_pre ,un_j_k   ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(   vn_dyn2_2_shapiro_pre ,vn_j_k   ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(  eta_dyn2_2_shapiro_pre ,eta_j_k  ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( etan_dyn2_2_shapiro_pre ,etan_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(  wet_dyn2_2_shapiro_pre ,wet_j_k  ); mem_fence(CLK_CHANNEL_MEM_FENCE); 
    write_channel_altera(hzero_dyn2_2_shapiro_pre ,hzero_j_k); 
    
    
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
__kernel void kernel_smache_dyn2_2_shapiro ( 
                                  ) {  
  //common parameters
  //-----------------
  const int arrsize   = SIZE; //Size of  input array(s)
  
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
  stypeDevice    un_buffer [ker_buffsize];
  stypeDevice    vn_buffer [ker_buffsize];
  stypeDevice   eta_buffer [ker_buffsize];
  stypeDevice  etan_buffer [ker_buffsize];
  stypeDevice   wet_buffer [ker_buffsize];
  stypeDevice hzero_buffer [ker_buffsize];

  //The tuple variables to emit
  stypeDevice     un_j_k;
  stypeDevice     vn_j_k;
  stypeDevice      h_j_k;
  stypeDevice    eta_j_k;
  stypeDevice   etan_j_k;
  stypeDevice etan_jm1_k;
  stypeDevice etan_j_km1;
  stypeDevice etan_j_kp1;
  stypeDevice etan_jp1_k;
  stypeDevice    wet_j_k;
  stypeDevice  wet_jm1_k;
  stypeDevice  wet_j_km1;
  stypeDevice  wet_j_kp1;
  stypeDevice  wet_jp1_k;
  stypeDevice  hzero_j_k;
  
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
         un_buffer[ker_buffsize-1] = read_channel_altera(   un_dyn2_2_shapiro_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
         vn_buffer[ker_buffsize-1] = read_channel_altera(   vn_dyn2_2_shapiro_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
        eta_buffer[ker_buffsize-1] = read_channel_altera(  eta_dyn2_2_shapiro_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
       etan_buffer[ker_buffsize-1] = read_channel_altera( etan_dyn2_2_shapiro_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
        wet_buffer[ker_buffsize-1] = read_channel_altera(  wet_dyn2_2_shapiro_pre);  mem_fence(CLK_CHANNEL_MEM_FENCE);
      hzero_buffer[ker_buffsize-1] = read_channel_altera(hzero_dyn2_2_shapiro_pre);  
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
  
      write_channel_altera (    un_j_k_dyn2_2_shapiro_post,     un_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (    vn_j_k_dyn2_2_shapiro_post,     vn_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (   eta_j_k_dyn2_2_shapiro_post,    eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (  etan_j_k_dyn2_2_shapiro_post,   etan_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_jm1_k_dyn2_2_shapiro_post, etan_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_j_km1_dyn2_2_shapiro_post, etan_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_j_kp1_dyn2_2_shapiro_post, etan_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_jp1_k_dyn2_2_shapiro_post, etan_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (   wet_j_k_dyn2_2_shapiro_post,    wet_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_jm1_k_dyn2_2_shapiro_post,  wet_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_km1_dyn2_2_shapiro_post,  wet_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_kp1_dyn2_2_shapiro_post,  wet_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_jp1_k_dyn2_2_shapiro_post,  wet_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( hzero_j_k_dyn2_2_shapiro_post,  hzero_j_k); 
    }//if(compindex>=0)
  }//for     
}



//------------------------------------------
// SHAPIRO KERNEL
//------------------------------------------
__kernel void kernel_shapiro  ( const stypeDevice eps 
                              ) {

  const int arrsize   = SIZE;   
  const int nloop     = arrsize;

  int compindex; 
                 
  int j, k;      

  //locals
  stypeDevice term1,term2,term3;

  
  //input from channels
  stypeDevice     un_j_k;
  stypeDevice     vn_j_k;
  stypeDevice    eta_j_k;
  stypeDevice   etan_j_k;
  stypeDevice etan_jm1_k;
  stypeDevice etan_j_km1;
  stypeDevice etan_j_kp1;
  stypeDevice etan_jp1_k;
  stypeDevice    wet_j_k;
  stypeDevice  wet_jm1_k;
  stypeDevice  wet_j_km1;
  stypeDevice  wet_j_kp1;
  stypeDevice  wet_jp1_k;
  stypeDevice  hzero_j_k;
 
  
  // The main loop //
  for (int count=0; count < nloop; count++) {  
    compindex = count;
    j = compindex/COLS;
    k = compindex%COLS;
      
        un_j_k = read_channel_altera (    un_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
        vn_j_k = read_channel_altera (    vn_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
       eta_j_k = read_channel_altera (   eta_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
      etan_j_k = read_channel_altera (  etan_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_jm1_k = read_channel_altera (etan_jm1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_km1 = read_channel_altera (etan_j_km1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_kp1 = read_channel_altera (etan_j_kp1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_jp1_k = read_channel_altera (etan_jp1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
       wet_j_k = read_channel_altera (   wet_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_jm1_k = read_channel_altera ( wet_jm1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_j_km1 = read_channel_altera ( wet_j_km1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_j_kp1 = read_channel_altera ( wet_j_kp1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_jp1_k = read_channel_altera ( wet_jp1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     hzero_j_k = read_channel_altera ( hzero_j_k_dyn2_2_shapiro_post); 
      
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
    
      write_channel_altera(eta_shapiro_2_udpate   , eta_j_k);   mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(un_shapiro_2_udpate    , un_j_k);    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(vn_shapiro_2_udpate    , vn_j_k);    mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(hzero_shapiro_2_udpate , hzero_j_k); 
 }//for l
}//()


//------------------------------------------
// UPDATES KERNEL
//------------------------------------------
__kernel void kernel_updates  ( stypeHost hmin
                              ) {
  int j, k, index;
  stypeDevice h_j_k;
  stypeDevice u_j_k;
  stypeDevice v_j_k;
  stypeDevice hzero_j_k ;
  stypeDevice eta_j_k   ;
  stypeDevice un_j_k    ;
  stypeDevice vn_j_k    ;
  stypeDevice wet_j_k   ;

  //we need to iterate over the entire space inc boundaries as we are reading from channel and not RAM
  for (int index=0; index < SIZE; index++) {     
      eta_j_k   = read_channel_altera(eta_shapiro_2_udpate);   mem_fence(CLK_CHANNEL_MEM_FENCE);  
      un_j_k    = read_channel_altera(un_shapiro_2_udpate);    mem_fence(CLK_CHANNEL_MEM_FENCE); 
      vn_j_k    = read_channel_altera(vn_shapiro_2_udpate);    mem_fence(CLK_CHANNEL_MEM_FENCE); 
      hzero_j_k = read_channel_altera(hzero_shapiro_2_udpate); 

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

      write_channel_altera(u_out_update   , u_j_k);   mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(v_out_update   , v_j_k);   mem_fence(CLK_CHANNEL_MEM_FENCE); 
      write_channel_altera(h_out_update   , h_j_k);   mem_fence(CLK_CHANNEL_MEM_FENCE); 
      write_channel_altera(eta_out_update , eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(wet_out_update , wet_j_k); 
  }//for
}//()


//------------------------------------------
// Write memory kernel
//------------------------------------------
kernel void kernel_mem_wr  (__global stypeDevice* restrict u
                           ,__global stypeDevice* restrict v
                           ,__global stypeDevice* restrict h
                           ,__global stypeDevice* restrict eta
                           ,__global stypeDevice* restrict wet
) {
  for (int index=0; index < SIZE; index++) {       
      stypeDevice u_new   = read_channel_altera(u_out_update);   mem_fence(CLK_CHANNEL_MEM_FENCE);
      stypeDevice v_new   = read_channel_altera(v_out_update);   mem_fence(CLK_CHANNEL_MEM_FENCE);
      stypeDevice h_new   = read_channel_altera(h_out_update);   mem_fence(CLK_CHANNEL_MEM_FENCE);
      stypeDevice eta_new = read_channel_altera(eta_out_update); mem_fence(CLK_CHANNEL_MEM_FENCE);
      stypeDevice wet_new = read_channel_altera(wet_out_update); 
      
      u[index] = u_new;
      v[index] = v_new;
      h[index] = h_new;
      eta[index] = eta_new;
      wet[index] = wet_new;
  }
}//()

