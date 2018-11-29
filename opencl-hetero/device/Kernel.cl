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
channel float un_dyn1_2_dyn2_pre;
channel float vn_dyn1_2_dyn2_pre;
channel float h_dyn1_2_dyn2_pre;
channel float eta_dyn1_2_dyn2_pre;
channel float etan_dyn1_2_dyn2_pre;
channel float wet_dyn1_2_dyn2_pre;
channel float hzero_dyn1_2_dyn2_pre;
channel float un_j_k_dyn1_2_dyn2_post;
channel float un_jm1_k_dyn1_2_dyn2_post;
channel float un_j_km1_dyn1_2_dyn2_post;
channel float vn_j_k_dyn1_2_dyn2_post;
channel float vn_jm1_k_dyn1_2_dyn2_post;
channel float vn_j_km1_dyn1_2_dyn2_post;
channel float h_j_k_dyn1_2_dyn2_post;
channel float h_jm1_k_dyn1_2_dyn2_post;
channel float h_j_km1_dyn1_2_dyn2_post;
channel float h_j_kp1_dyn1_2_dyn2_post;
channel float h_jp1_k_dyn1_2_dyn2_post;
channel float eta_j_k_dyn1_2_dyn2_post;
channel float etan_j_k_dyn1_2_dyn2_post;
channel float wet_j_k_dyn1_2_dyn2_post;
channel float hzero_j_k_dyn1_2_dyn2_post;
channel float un_dyn2_2_shapiro_pre ;
channel float vn_dyn2_2_shapiro_pre ;
channel float eta_dyn2_2_shapiro_pre ;
channel float etan_dyn2_2_shapiro_pre ;
channel float wet_dyn2_2_shapiro_pre ;
channel float hzero_dyn2_2_shapiro_pre ;
channel float un_j_k_dyn2_2_shapiro_post;
channel float vn_j_k_dyn2_2_shapiro_post;
channel float eta_j_k_dyn2_2_shapiro_post;
channel float etan_j_k_dyn2_2_shapiro_post;
channel float etan_jm1_k_dyn2_2_shapiro_post;
channel float etan_j_km1_dyn2_2_shapiro_post;
channel float etan_j_kp1_dyn2_2_shapiro_post;
channel float etan_jp1_k_dyn2_2_shapiro_post;
channel float wet_j_k_dyn2_2_shapiro_post;
channel float wet_jm1_k_dyn2_2_shapiro_post;
channel float wet_j_km1_dyn2_2_shapiro_post;
channel float wet_j_kp1_dyn2_2_shapiro_post;
channel float wet_jp1_k_dyn2_2_shapiro_post;
channel float hzero_j_k_dyn2_2_shapiro_post;
channel float eta_shapiro_2_udpate ;
channel float un_shapiro_2_udpate ;
channel float vn_shapiro_2_udpate ;
channel float h_shapiro_2_udpate ;
channel float hzero_shapiro_2_udpate ;
channel float u_out_update ;
channel float v_out_update ;
channel float h_out_update ;
channel float eta_out_update ;
channel float wet_out_update ;
__kernel void kernel_mem_rd( __global float * restrict u
                       , __global float * restrict v
                       , __global float * restrict h
                       , __global float * restrict eta
                       , __global float * restrict etan
                       , __global float * restrict wet
                       , __global float * restrict hzero
                      ) {
  uint j, k, index;
  for (index=0; index < (20*20); index++) {
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
  const int arrsize = (20*20);
  const int ker_maxoffpos = 20;
  const int ker_maxoffneg = 0;
  const int nloop = arrsize + ker_maxoffpos;
  const int ker_buffsize = ker_maxoffpos + ker_maxoffneg + 1;
  const int ind_j_k = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k = ind_j_k + 20;
  const int ind_j_kp1 = ind_j_k + 1;
  const int ind_jm1_k = ind_j_k - 20;
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
  const int arrsize = (20*20);
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
    j = compindex/20;
    k = compindex%20;
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
    if ((j>=1) && (k>=1) && (j<= 20 -2) && (k<=20 -2)) {
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
    write_channel_altera( un_dyn1_2_dyn2_pre ,un_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( vn_dyn1_2_dyn2_pre ,vn_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( h_dyn1_2_dyn2_pre ,h_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( eta_dyn1_2_dyn2_pre ,eta_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( etan_dyn1_2_dyn2_pre ,etan_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( wet_dyn1_2_dyn2_pre ,wet_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(hzero_dyn1_2_dyn2_pre ,hzero_j_k);
 }
}
__kernel void kernel_smache_dyn1_2_dyn2 (
                                  ) {
  const int arrsize = (20*20);
  const int ker_maxoffpos = 20;
  const int ker_maxoffneg = 20;
  const int nloop = arrsize + ker_maxoffpos;
  const int ker_buffsize = ker_maxoffpos + ker_maxoffneg + 1;
  const int ind_j_k = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k = ind_j_k + 20;
  const int ind_j_kp1 = ind_j_k + 1;
  const int ind_jm1_k = ind_j_k - 20;
  const int ind_j_km1 = ind_j_k - 1;
  float un_buffer [ker_buffsize];
  float vn_buffer [ker_buffsize];
  float h_buffer [ker_buffsize];
  float eta_buffer [ker_buffsize];
  float etan_buffer [ker_buffsize];
  float wet_buffer [ker_buffsize];
  float hzero_buffer [ker_buffsize];
  float un_j_k;
  float un_jm1_k;
  float un_j_km1;
  float vn_j_k;
  float vn_jm1_k;
  float vn_j_km1;
  float h_j_k;
  float h_jm1_k;
  float h_j_km1;
  float h_j_kp1;
  float h_jp1_k;
  float eta_j_k;
  float etan_j_k;
  float wet_j_k;
  float hzero_j_k;
  for (int count=0; count < nloop ; count++) {
     int compindex = count - ker_maxoffpos;
#pragma unroll
     for (int i = 0; i < ker_buffsize-1 ; ++i) {
             un_buffer[i] = un_buffer[i + 1];
             vn_buffer[i] = vn_buffer[i + 1];
              h_buffer[i] = h_buffer[i + 1];
            eta_buffer[i] = eta_buffer[i + 1];
           etan_buffer[i] = etan_buffer[i + 1];
            wet_buffer[i] = wet_buffer[i + 1];
          hzero_buffer[i] = hzero_buffer[i + 1];
      }
    if(count < arrsize) {
         un_buffer[ker_buffsize-1] = read_channel_altera( un_dyn1_2_dyn2_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
         vn_buffer[ker_buffsize-1] = read_channel_altera( vn_dyn1_2_dyn2_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
          h_buffer[ker_buffsize-1] = read_channel_altera( h_dyn1_2_dyn2_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
        eta_buffer[ker_buffsize-1] = read_channel_altera( eta_dyn1_2_dyn2_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
       etan_buffer[ker_buffsize-1] = read_channel_altera( etan_dyn1_2_dyn2_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
        wet_buffer[ker_buffsize-1] = read_channel_altera( wet_dyn1_2_dyn2_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
      hzero_buffer[ker_buffsize-1] = read_channel_altera(hzero_dyn1_2_dyn2_pre);
    }
    if(compindex>=0) {
         un_j_k = un_buffer[ind_j_k];
       un_jm1_k = un_buffer[ind_jm1_k];
       un_j_km1 = un_buffer[ind_j_km1];
         vn_j_k = vn_buffer[ind_j_k];
       vn_jm1_k = vn_buffer[ind_jm1_k];
       vn_j_km1 = vn_buffer[ind_j_km1];
          h_j_k = h_buffer[ind_j_k];
        h_jm1_k = h_buffer[ind_jm1_k];
        h_j_km1 = h_buffer[ind_j_km1];
        h_j_kp1 = h_buffer[ind_j_kp1];
        h_jp1_k = h_buffer[ind_jp1_k];
        eta_j_k = eta_buffer[ind_j_k];
       etan_j_k = etan_buffer[ind_j_k];
        wet_j_k = wet_buffer[ind_j_k];
      hzero_j_k = hzero_buffer[ind_j_k];
      write_channel_altera ( un_j_k_dyn1_2_dyn2_post, un_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( un_jm1_k_dyn1_2_dyn2_post, un_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( un_j_km1_dyn1_2_dyn2_post, un_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( vn_j_k_dyn1_2_dyn2_post, vn_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( vn_jm1_k_dyn1_2_dyn2_post, vn_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( vn_j_km1_dyn1_2_dyn2_post, vn_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( h_j_k_dyn1_2_dyn2_post, h_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( h_jm1_k_dyn1_2_dyn2_post, h_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( h_j_km1_dyn1_2_dyn2_post, h_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( h_j_kp1_dyn1_2_dyn2_post, h_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( h_jp1_k_dyn1_2_dyn2_post, h_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( eta_j_k_dyn1_2_dyn2_post, eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( etan_j_k_dyn1_2_dyn2_post, etan_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_k_dyn1_2_dyn2_post, wet_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (hzero_j_k_dyn1_2_dyn2_post, hzero_j_k);
    }
  }
}
__kernel void kernel_dyn2( const float dt
                         , const float dx
                         , const float dy
                         ) {
  const int arrsize = (20*20);
  const int nloop = arrsize;
  int compindex;
  int j, k;
  float hue;
  float huw;
  float hwp;
  float hwn;
  float hen;
  float hep;
  float hvn;
  float hvs;
  float hsp;
  float hsn;
  float hnn;
  float hnp;
  float un_j_k;
  float un_jm1_k;
  float un_j_km1;
  float vn_j_k;
  float vn_jm1_k;
  float vn_j_km1;
  float h_j_k;
  float h_jm1_k;
  float h_j_km1;
  float h_j_kp1;
  float h_jp1_k;
  float eta_j_k;
  float etan_j_k;
  float wet_j_k;
  float hzero_j_k;
  for (int count=0; count < nloop; count++) {
    compindex = count;
    j = compindex/20;
    k = compindex%20;
      un_j_k = read_channel_altera ( un_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    un_jm1_k = read_channel_altera ( un_jm1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    un_j_km1 = read_channel_altera ( un_j_km1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
      vn_j_k = read_channel_altera ( vn_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    vn_jm1_k = read_channel_altera ( vn_jm1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    vn_j_km1 = read_channel_altera ( vn_j_km1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
       h_j_k = read_channel_altera ( h_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_jm1_k = read_channel_altera ( h_jm1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_j_km1 = read_channel_altera ( h_j_km1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_j_kp1 = read_channel_altera ( h_j_kp1_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     h_jp1_k = read_channel_altera ( h_jp1_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     eta_j_k = read_channel_altera ( eta_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_k = read_channel_altera ( etan_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_j_k = read_channel_altera ( wet_j_k_dyn1_2_dyn2_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    hzero_j_k = read_channel_altera (hzero_j_k_dyn1_2_dyn2_post);
    if ((j>=1) && (k>=1) && (j<= 20 -2) && (k<=20 -2)) {
      hep = 0.5*( un_j_k + fabs(un_j_k) ) * h_j_k;
      hen = 0.5*( un_j_k - fabs(un_j_k) ) * h_j_kp1;
      hue = hep+hen;
      hwp = 0.5*( un_j_km1 + fabs(un_j_km1) ) * h_j_km1;
      hwn = 0.5*( un_j_km1 - fabs(un_j_km1) ) * h_j_k;
      huw = hwp+hwn;
      hnp = 0.5*( vn_j_k + fabs(vn_j_k) ) * h_j_k;
      hnn = 0.5*( vn_j_k - fabs(vn_j_k) ) * h_jp1_k;
      hvn = hnp+hnn;
      hsp = 0.5*( vn_jm1_k + fabs(vn_jm1_k) ) * h_jm1_k;
      hsn = 0.5*( vn_jm1_k - fabs(vn_jm1_k) ) * h_j_k;
      hvs = hsp+hsn;
      etan_j_k = eta_j_k
                - dt*(hue-huw)/dx
                - dt*(hvn-hvs)/dy;
    }
    write_channel_altera( un_dyn2_2_shapiro_pre ,un_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( vn_dyn2_2_shapiro_pre ,vn_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( eta_dyn2_2_shapiro_pre ,eta_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( etan_dyn2_2_shapiro_pre ,etan_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera( wet_dyn2_2_shapiro_pre ,wet_j_k ); mem_fence(CLK_CHANNEL_MEM_FENCE);
    write_channel_altera(hzero_dyn2_2_shapiro_pre ,hzero_j_k);
  }
}
__kernel void kernel_smache_dyn2_2_shapiro (
                                  ) {
  const int arrsize = (20*20);
  const int ker_maxoffpos = 20;
  const int ker_maxoffneg = 20;
  const int nloop = arrsize + ker_maxoffpos;
  const int ker_buffsize = ker_maxoffpos + ker_maxoffneg + 1;
  const int ind_j_k = ker_buffsize - 1 - ker_maxoffpos;
  const int ind_jp1_k = ind_j_k + 20;
  const int ind_j_kp1 = ind_j_k + 1;
  const int ind_jm1_k = ind_j_k - 20;
  const int ind_j_km1 = ind_j_k - 1;
  float un_buffer [ker_buffsize];
  float vn_buffer [ker_buffsize];
  float eta_buffer [ker_buffsize];
  float etan_buffer [ker_buffsize];
  float wet_buffer [ker_buffsize];
  float hzero_buffer [ker_buffsize];
  float un_j_k;
  float vn_j_k;
  float h_j_k;
  float eta_j_k;
  float etan_j_k;
  float etan_jm1_k;
  float etan_j_km1;
  float etan_j_kp1;
  float etan_jp1_k;
  float wet_j_k;
  float wet_jm1_k;
  float wet_j_km1;
  float wet_j_kp1;
  float wet_jp1_k;
  float hzero_j_k;
  for (int count=0; count < nloop ; count++) {
     int compindex = count - ker_maxoffpos;
#pragma unroll
     for (int i = 0; i < ker_buffsize-1 ; ++i) {
             un_buffer[i] = un_buffer[i + 1];
             vn_buffer[i] = vn_buffer[i + 1];
            eta_buffer[i] = eta_buffer[i + 1];
           etan_buffer[i] = etan_buffer[i + 1];
            wet_buffer[i] = wet_buffer[i + 1];
          hzero_buffer[i] = hzero_buffer[i + 1];
      }
    if(count < arrsize) {
         un_buffer[ker_buffsize-1] = read_channel_altera( un_dyn2_2_shapiro_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
         vn_buffer[ker_buffsize-1] = read_channel_altera( vn_dyn2_2_shapiro_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
        eta_buffer[ker_buffsize-1] = read_channel_altera( eta_dyn2_2_shapiro_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
       etan_buffer[ker_buffsize-1] = read_channel_altera( etan_dyn2_2_shapiro_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
        wet_buffer[ker_buffsize-1] = read_channel_altera( wet_dyn2_2_shapiro_pre); mem_fence(CLK_CHANNEL_MEM_FENCE);
      hzero_buffer[ker_buffsize-1] = read_channel_altera(hzero_dyn2_2_shapiro_pre);
    }
    if(compindex>=0) {
         un_j_k = un_buffer[ind_j_k];
         vn_j_k = vn_buffer[ind_j_k];
        eta_j_k = eta_buffer[ind_j_k];
       etan_j_k = etan_buffer[ind_j_k];
     etan_jm1_k = etan_buffer[ind_jm1_k];
     etan_j_km1 = etan_buffer[ind_j_km1];
     etan_j_kp1 = etan_buffer[ind_j_kp1];
     etan_jp1_k = etan_buffer[ind_jp1_k];
        wet_j_k = wet_buffer[ind_j_k];
      wet_jm1_k = wet_buffer[ind_jm1_k];
      wet_j_km1 = wet_buffer[ind_j_km1];
      wet_j_kp1 = wet_buffer[ind_j_kp1];
      wet_jp1_k = wet_buffer[ind_jp1_k];
      hzero_j_k = hzero_buffer[ind_j_k];
      write_channel_altera ( un_j_k_dyn2_2_shapiro_post, un_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( vn_j_k_dyn2_2_shapiro_post, vn_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( eta_j_k_dyn2_2_shapiro_post, eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( etan_j_k_dyn2_2_shapiro_post, etan_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_jm1_k_dyn2_2_shapiro_post, etan_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_j_km1_dyn2_2_shapiro_post, etan_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_j_kp1_dyn2_2_shapiro_post, etan_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera (etan_jp1_k_dyn2_2_shapiro_post, etan_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_k_dyn2_2_shapiro_post, wet_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_jm1_k_dyn2_2_shapiro_post, wet_jm1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_km1_dyn2_2_shapiro_post, wet_j_km1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_j_kp1_dyn2_2_shapiro_post, wet_j_kp1); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( wet_jp1_k_dyn2_2_shapiro_post, wet_jp1_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera ( hzero_j_k_dyn2_2_shapiro_post, hzero_j_k);
    }
  }
}
__kernel void kernel_shapiro ( const float eps
                              ) {
  const int arrsize = (20*20);
  const int nloop = arrsize;
  int compindex;
  int j, k;
  float term1,term2,term3;
  float un_j_k;
  float vn_j_k;
  float eta_j_k;
  float etan_j_k;
  float etan_jm1_k;
  float etan_j_km1;
  float etan_j_kp1;
  float etan_jp1_k;
  float wet_j_k;
  float wet_jm1_k;
  float wet_j_km1;
  float wet_j_kp1;
  float wet_jp1_k;
  float hzero_j_k;
  for (int count=0; count < nloop; count++) {
    compindex = count;
    j = compindex/20;
    k = compindex%20;
        un_j_k = read_channel_altera ( un_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
        vn_j_k = read_channel_altera ( vn_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
       eta_j_k = read_channel_altera ( eta_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
      etan_j_k = read_channel_altera ( etan_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_jm1_k = read_channel_altera (etan_jm1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_km1 = read_channel_altera (etan_j_km1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_j_kp1 = read_channel_altera (etan_j_kp1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
    etan_jp1_k = read_channel_altera (etan_jp1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
       wet_j_k = read_channel_altera ( wet_j_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_jm1_k = read_channel_altera ( wet_jm1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_j_km1 = read_channel_altera ( wet_j_km1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_j_kp1 = read_channel_altera ( wet_j_kp1_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     wet_jp1_k = read_channel_altera ( wet_jp1_k_dyn2_2_shapiro_post); mem_fence(CLK_CHANNEL_MEM_FENCE);
     hzero_j_k = read_channel_altera ( hzero_j_k_dyn2_2_shapiro_post);
      if ((j>=1) && (k>=1) && (j<= 20 -2) && (k<=20 -2)) {
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
        }
        else {
          eta_j_k = etan_j_k;
        }
      }
      write_channel_altera(eta_shapiro_2_udpate , eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(un_shapiro_2_udpate , un_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(vn_shapiro_2_udpate , vn_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(hzero_shapiro_2_udpate , hzero_j_k);
 }
}
__kernel void kernel_updates ( float hmin
                              ) {
  int j, k, index;
  float h_j_k;
  float u_j_k;
  float v_j_k;
  float hzero_j_k ;
  float eta_j_k ;
  float un_j_k ;
  float vn_j_k ;
  float wet_j_k ;
  for (int index=0; index < (20*20); index++) {
      eta_j_k = read_channel_altera(eta_shapiro_2_udpate); mem_fence(CLK_CHANNEL_MEM_FENCE);
      un_j_k = read_channel_altera(un_shapiro_2_udpate); mem_fence(CLK_CHANNEL_MEM_FENCE);
      vn_j_k = read_channel_altera(vn_shapiro_2_udpate); mem_fence(CLK_CHANNEL_MEM_FENCE);
      hzero_j_k = read_channel_altera(hzero_shapiro_2_udpate);
      int j = index/20;
      int k = index%20;
      h_j_k = hzero_j_k
            + eta_j_k;
      wet_j_k = 1;
      if ( h_j_k < hmin )
            wet_j_k = 0;
      u_j_k = un_j_k;
      v_j_k = vn_j_k;
      write_channel_altera(u_out_update , u_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(v_out_update , v_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(h_out_update , h_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(eta_out_update , eta_j_k); mem_fence(CLK_CHANNEL_MEM_FENCE);
      write_channel_altera(wet_out_update , wet_j_k);
  }
}
kernel void kernel_mem_wr (__global float* restrict u
                           ,__global float* restrict v
                           ,__global float* restrict h
                           ,__global float* restrict eta
                           ,__global float* restrict wet
) {
  for (int index=0; index < (20*20); index++) {
      float u_new = read_channel_altera(u_out_update); mem_fence(CLK_CHANNEL_MEM_FENCE);
      float v_new = read_channel_altera(v_out_update); mem_fence(CLK_CHANNEL_MEM_FENCE);
      float h_new = read_channel_altera(h_out_update); mem_fence(CLK_CHANNEL_MEM_FENCE);
      float eta_new = read_channel_altera(eta_out_update); mem_fence(CLK_CHANNEL_MEM_FENCE);
      float wet_new = read_channel_altera(wet_out_update);
      u[index] = u_new;
      v[index] = v_new;
      h[index] = h_new;
      eta[index] = eta_new;
      wet[index] = wet_new;
  }
}
