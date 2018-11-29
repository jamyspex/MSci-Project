#pragma OPENCL EXTENSION cl_altera_channels : enable
channel float u_memrd_2_dyn1_pre;
channel float v_memrd_2_dyn1_pre;
channel float h_memrd_2_dyn1_pre;
channel float eta_memrd_2_dyn1_pre;
channel float etan_memrd_2_dyn1_pre;
channel float wet_memrd_2_dyn1_pre;
channel float hzero_memrd_2_dyn1_pre;
channel float u_j_k_memrd_2_dyn1_post;
channel float v_j_k_memrd_2_dyn1_post;
channel float h_j_k_memrd_2_dyn1_post;
channel float eta_j_k_memrd_2_dyn1_post;
channel float eta_j_kp1_memrd_2_dyn1_post;
channel float eta_jp1_k_memrd_2_dyn1_post;
channel float etan_j_k_memrd_2_dyn1_post;
channel float wet_j_k_memrd_2_dyn1_post;
channel float wet_j_kp1_memrd_2_dyn1_post;
channel float wet_jp1_k_memrd_2_dyn1_post;
channel float hzero_j_k_memrd_2_dyn1_post;
channel float un_dyn1_2_dyn2_pre_out __attribute__((io( "un_dyn1_2_dyn2_pre_out")));
channel float vn_dyn1_2_dyn2_pre_out __attribute__((io( "vn_dyn1_2_dyn2_pre_out")));
channel float h_dyn1_2_dyn2_pre_out __attribute__((io( "h_dyn1_2_dyn2_pre_out")));
channel float eta_dyn1_2_dyn2_pre_out __attribute__((io( "eta_dyn1_2_dyn2_pre_out")));
channel float etan_dyn1_2_dyn2_pre_out __attribute__((io( "etan_dyn1_2_dyn2_pre_out")));
channel float wet_dyn1_2_dyn2_pre_out __attribute__((io( "wet_dyn1_2_dyn2_pre_out")));
channel float hzero_dyn1_2_dyn2_pre_out __attribute__((io("hzero_dyn1_2_dyn2_pre_out")));
__kernel void kernel_mem_rd( __global float * restrict u
                       , __global float * restrict v
                       , __global float * restrict h
                       , __global float * restrict eta
                       , __global float * restrict etan
                       , __global float * restrict wet
                       , __global float * restrict hzero
                      ) {
  uint j, k, index;
  for (index=0; index < (50*50); index++) {
      float u_data = u[index];
      float v_data = v[index];
      float h_data = h[index];
      float eta_data = eta[index];
      float etan_data = etan[index];
      float wet_data = wet[index];
      float hzero_data= hzero[index];
      write_channel_altera(u_memrd_2_dyn1_pre ,u_data ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(v_memrd_2_dyn1_pre ,v_data ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(h_memrd_2_dyn1_pre ,h_data ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(eta_memrd_2_dyn1_pre ,eta_data ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(etan_memrd_2_dyn1_pre ,etan_data ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(wet_memrd_2_dyn1_pre ,wet_data ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(hzero_memrd_2_dyn1_pre ,hzero_data );
  }
}
__kernel void kernel_smache_memrd_2_dyn1 (){
  const int arrsize = (50*50);
  const int ker_maxoffpos = 50;
  const int ker_maxoffneg = 0;
  const int nloop = arrsize + ker_maxoffpos;
  const int ker_buffsize = ker_maxoffpos + ker_maxoffneg + 1;
  const int ind_j_k = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k = ind_j_k + 50;
  const int ind_j_kp1 = ind_j_k + 1;
  const int ind_jm1_k = ind_j_k - 50;
  const int ind_j_km1 = ind_j_k - 1;
  float u_buffer [ker_buffsize];
  float v_buffer [ker_buffsize];
  float h_buffer [ker_buffsize];
  float eta_buffer [ker_buffsize];
  float etan_buffer [ker_buffsize];
  float wet_buffer [ker_buffsize];
  float hzero_buffer [ker_buffsize];
  float u_j_k;
  float v_j_k;
  float h_j_k;
  float eta_j_k;
  float eta_j_kp1;
  float eta_jp1_k;
  float etan_j_k;
  float wet_j_k;
  float wet_j_kp1;
  float wet_jp1_k;
  float hzero_j_k;
  int count, compindex, count_backup;
  for (count=0; count < nloop ; count++) {
    compindex = count - ker_maxoffpos;
#pragma unroll
     for (int i = 0; i < ker_buffsize-1 ; ++i) {
              u_buffer[i] = u_buffer[i + 1];
              v_buffer[i] = v_buffer[i + 1];
              h_buffer[i] = h_buffer[i + 1];
            eta_buffer[i] = eta_buffer[i + 1];
           etan_buffer[i] = etan_buffer[i + 1];
            wet_buffer[i] = wet_buffer[i + 1];
          hzero_buffer[i] = hzero_buffer[i + 1];
      }
    if(count < arrsize) {
         u_buffer[ker_buffsize-1] = read_channel_altera(u_memrd_2_dyn1_pre ); mem_fence(CLK_CHANNEL_MEM_FENCE);
         v_buffer[ker_buffsize-1] = read_channel_altera(v_memrd_2_dyn1_pre ); mem_fence(CLK_CHANNEL_MEM_FENCE);
         h_buffer[ker_buffsize-1] = read_channel_altera(h_memrd_2_dyn1_pre ); mem_fence(CLK_CHANNEL_MEM_FENCE);
       eta_buffer[ker_buffsize-1] = read_channel_altera(eta_memrd_2_dyn1_pre ); mem_fence(CLK_CHANNEL_MEM_FENCE);
      etan_buffer[ker_buffsize-1] = read_channel_altera(etan_memrd_2_dyn1_pre ); mem_fence(CLK_CHANNEL_MEM_FENCE);
       wet_buffer[ker_buffsize-1] = read_channel_altera(wet_memrd_2_dyn1_pre ); mem_fence(CLK_CHANNEL_MEM_FENCE);
     hzero_buffer[ker_buffsize-1] = read_channel_altera(hzero_memrd_2_dyn1_pre );
   }
    if(compindex>=0) {
          u_j_k = u_buffer[ind_j_k];
          v_j_k = v_buffer[ind_j_k];
          h_j_k = h_buffer[ind_j_k];
        eta_j_k = eta_buffer[ind_j_k];
      eta_j_kp1 = eta_buffer[ind_j_kp1];
      eta_jp1_k = eta_buffer[ind_jp1_k];
       etan_j_k = etan_buffer[ind_j_k];
        wet_j_k = wet_buffer[ind_j_k];
      wet_j_kp1 = wet_buffer[ind_j_kp1];
      wet_jp1_k = wet_buffer[ind_jp1_k];
      hzero_j_k = hzero_buffer[ind_j_k];
      write_channel_altera ( u_j_k_memrd_2_dyn1_post, u_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( v_j_k_memrd_2_dyn1_post, v_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( h_j_k_memrd_2_dyn1_post, h_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( eta_j_k_memrd_2_dyn1_post, eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (eta_j_kp1_memrd_2_dyn1_post, eta_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (eta_jp1_k_memrd_2_dyn1_post, eta_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( etan_j_k_memrd_2_dyn1_post, etan_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_k_memrd_2_dyn1_post, wet_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (wet_j_kp1_memrd_2_dyn1_post, wet_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (wet_jp1_k_memrd_2_dyn1_post, wet_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (hzero_j_k_memrd_2_dyn1_post, hzero_j_k);
    }
  }
}
__kernel void kernel_dyn1( const float dt
                         , const float dx
                         , const float dy
                         , const float g
                         ) {
  float du;
  float dv;
  float uu;
  float vv;
  float duu;
  float dvv;
  const int arrsize = (50*50);
  const int nloop = arrsize;
  int compindex;
  int j, k;
  float u_j_k ;
  float v_j_k ;
  float h_j_k ;
  float eta_j_k ;
  float eta_j_kp1 ;
  float eta_jp1_k ;
  float etan_j_k ;
  float wet_j_k ;
  float wet_j_kp1 ;
  float wet_jp1_k ;
  float hzero_j_k ;
  for (int count=0; count < nloop ; count++) {
    compindex = count;
    j = compindex/50;
    k = compindex%50;
    u_j_k = read_channel_altera( u_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    v_j_k = read_channel_altera( v_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    h_j_k = read_channel_altera( h_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    eta_j_k = read_channel_altera( eta_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    eta_j_kp1 = read_channel_altera(eta_j_kp1_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    eta_jp1_k = read_channel_altera(eta_jp1_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_k = read_channel_altera( etan_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    wet_j_k = read_channel_altera( wet_j_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    wet_j_kp1 = read_channel_altera(wet_j_kp1_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    wet_jp1_k = read_channel_altera(wet_jp1_k_memrd_2_dyn1_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    hzero_j_k = read_channel_altera(hzero_j_k_memrd_2_dyn1_post);
    float un_j_k = 0.0;
    float vn_j_k = 0.0;
    if ((j>=1) && (k>=1) && (j<= 50 -2) && (k<=50 -2)) {
      duu = -dt
           * g
           * ( eta_j_kp1
             - eta_j_k
             )
           / dx;
      dvv = -dt
           * g
           * ( eta_jp1_k
             - eta_j_k
             )
           / dy;
      uu = u_j_k;
      if ( ( (wet_j_k == 1)
              && ( (wet_j_kp1 == 1) || (duu > 0.0)))
         || ( (wet_j_kp1 == 1) && (duu < 0.0))
         ){
          un_j_k = uu+duu;
      }
      vv = v_j_k;
      if ( ( (wet_j_k == 1)
             && ( (wet_jp1_k == 1) || (dvv > 0.0)))
         || ((wet_jp1_k == 1) && (dvv < 0.0))
         ){
          vn_j_k = vv+dvv;
      }
    }
    write_channel_altera( un_dyn1_2_dyn2_pre_out ,un_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( vn_dyn1_2_dyn2_pre_out ,vn_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( h_dyn1_2_dyn2_pre_out ,h_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( eta_dyn1_2_dyn2_pre_out ,eta_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( etan_dyn1_2_dyn2_pre_out ,etan_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( wet_dyn1_2_dyn2_pre_out ,wet_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(hzero_dyn1_2_dyn2_pre_out ,hzero_j_k);
 }
}
