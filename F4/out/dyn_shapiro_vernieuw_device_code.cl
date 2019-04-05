





__attribute__((always_inline))
inline unsigned int F3D2C(
        unsigned int i_rng,unsigned int j_rng, // ranges, i.e. (hb-lb)+1
        int i_lb, int j_lb, int k_lb, // lower bounds
        int ix, int jx, int kx
        ) {
    return (i_rng*j_rng*(kx-k_lb)+i_rng*(jx-j_lb)+ix-i_lb);
}

__attribute__((always_inline))
inline unsigned int F2D2C(
        unsigned int i_rng, // ranges, i.e. (hb-lb)+1
        int i_lb, int j_lb, // lower bounds
        int ix, int jx
        ) {
    return (i_rng*(jx-j_lb)+ix-i_lb);
}


__attribute__((always_inline))
inline unsigned int F1D2C(
        int i_lb, // lower bounds
        int ix
        ) {
    return ix-i_lb;
}

__attribute__((always_inline))
inline unsigned int F4D2C(
        unsigned int i_rng,unsigned int j_rng, unsigned int k_rng, // ranges, i.e. (hb-lb)+1
        int i_lb, int j_lb, int k_lb, int l_lb, // lower bounds
        int ix, int jx, int kx, int lx
        ) {
    return (i_rng*j_rng*k_rng*(lx-l_lb)+
            i_rng*j_rng*(kx-k_lb)+
            i_rng*(jx-j_lb)+
            ix-i_lb
            );
}





        pipe float dyn_0__dyn_1__du_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_0__dyn_1__dv_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_0_smart_cache__dyn_0__eta_j_kp1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_0_smart_cache__dyn_0__eta_jp1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_0_smart_cache__dyn_0__eta_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_0_eta_j_k_reader__dyn_0_smart_cache__eta_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1__dyn_2_smart_cache__un_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1__dyn_2_smart_cache__vn_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1_smart_cache__dyn_1__wet_j_kp1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1_smart_cache__dyn_1__wet_jp1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1_smart_cache__dyn_1__wet_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1_wet_j_k_reader__dyn_1_smart_cache__wet_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1_u_j_k_reader__dyn_1__u_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_1_v_j_k_reader__dyn_1__v_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2__shapiro_smart_cache__vn_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2__shapiro_smart_cache__un_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2__shapiro_smart_cache__etan_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__h_jm1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__h_j_kp1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__h_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__h_jp1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__h_j_km1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__un_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__un_j_km1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__vn_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_smart_cache__dyn_2__vn_jm1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_h_j_k_reader__dyn_2_smart_cache__h_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float dyn_2_eta_j_k_reader__dyn_2__eta_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro__vernieuw__eta_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro__vernieuw__un_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro__vernieuw__vn_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__vn_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__un_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__etan_jm1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__etan_j_kp1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__etan_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__etan_jp1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__etan_j_km1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__wet_jm1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__wet_j_kp1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__wet_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__wet_jp1_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_smart_cache__shapiro__wet_j_km1__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float shapiro_wet_j_k_reader__shapiro_smart_cache__wet_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float vernieuw__vernieuw_output_writer__h_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float vernieuw__vernieuw_output_writer__u_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float vernieuw__vernieuw_output_writer__v_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float vernieuw__vernieuw_output_writer__wet_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float vernieuw_h_j_k_reader__vernieuw__h_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));
        pipe float vernieuw_hzero_j_k_reader__vernieuw__hzero_j_k__pipe __attribute__((xcl_reqd_pipe_depth(32)));

__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_0_eta_j_k_reader(float *eta) {

      float eta_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                eta_j_k = eta[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(dyn_0_eta_j_k_reader__dyn_0_smart_cache__eta_j_k__pipe,&eta_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_0_smart_cache() {

      float eta_read_in;
      float eta_buffer[503];
      float eta_j_kp1;
      float eta_jp1_k;
      float eta_j_k;
      int i;
      int count;
      int compindex;
      const int nloop = 252507;
      const int smartcachesize = 503;
      const int maxpositiveoffset = 503;
      const int maxnegativeoffset = 0;
    for (count = 1;count <= nloop;count += 1) {
                compindex = count-maxpositiveoffset;

#pragma unroll
        for (i = 1;i <= smartcachesize-1;i += 1) {
                                eta_buffer[F1D2C(1 , i)] = eta_buffer[F1D2C(1 , i+1)];
        }
        if (count<smartcachesize) {
                                read_pipe(dyn_0_eta_j_k_reader__dyn_0_smart_cache__eta_j_k__pipe,&eta_read_in);
                                eta_buffer[F1D2C(1 , 503)] = eta_read_in;
        }
        if (compindex>=0) {
                                eta_j_k = *eta_buffer;
                                write_pipe(dyn_0_smart_cache__dyn_0__eta_j_k__pipe,&eta_j_k);
                                eta_j_kp1 = eta_buffer[F1D2C(1 , 503)];
                                write_pipe(dyn_0_smart_cache__dyn_0__eta_j_kp1__pipe,&eta_j_kp1);
                                eta_jp1_k = eta_buffer[F1D2C(1 , 2)];
                                write_pipe(dyn_0_smart_cache__dyn_0__eta_jp1_k__pipe,&eta_jp1_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_0(float *dt,float *dx,float *dy,float *g) {

      const int nloop = 252004;
      int count;
      float du_j_k;
      float dv_j_k;
      float eta_j_kp1;
      float eta_jp1_k;
      float eta_j_k;
      int j;
      int k;
    for (count = 1;count <= nloop;count += 1) {
                read_pipe(dyn_0_smart_cache__dyn_0__eta_j_k__pipe,&eta_j_k);
                read_pipe(dyn_0_smart_cache__dyn_0__eta_j_kp1__pipe,&eta_j_kp1);
                read_pipe(dyn_0_smart_cache__dyn_0__eta_jp1_k__pipe,&eta_jp1_k);
                j = count*1.0/502;
                k = count%502;
        if ((j>=1)&&j<=500&&k>=1&&k<=500) {
                                du_j_k = -(*dt)*(*g)*(eta_j_kp1-eta_j_k)*1.0/(*dx);
                                dv_j_k = -(*dt)*(*g)*(eta_jp1_k-eta_j_k)*1.0/(*dy);
        }
                write_pipe(dyn_0__dyn_1__du_j_k__pipe,&du_j_k);
                write_pipe(dyn_0__dyn_1__dv_j_k__pipe,&dv_j_k);
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_1_wet_j_k_reader(float *wet) {

      float wet_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                wet_j_k = wet[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(dyn_1_wet_j_k_reader__dyn_1_smart_cache__wet_j_k__pipe,&wet_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_1_u_j_k_reader(float *u) {

      float u_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                u_j_k = u[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(dyn_1_u_j_k_reader__dyn_1__u_j_k__pipe,&u_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_1_v_j_k_reader(float *v) {

      float v_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                v_j_k = v[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(dyn_1_v_j_k_reader__dyn_1__v_j_k__pipe,&v_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_1_smart_cache() {

      float wet_read_in;
      float wet_buffer[503];
      float wet_j_kp1;
      float wet_jp1_k;
      float wet_j_k;
      int i;
      int count;
      int compindex;
      const int nloop = 252507;
      const int smartcachesize = 503;
      const int maxpositiveoffset = 503;
      const int maxnegativeoffset = 0;
    for (count = 1;count <= nloop;count += 1) {
                compindex = count-maxpositiveoffset;

#pragma unroll
        for (i = 1;i <= smartcachesize-1;i += 1) {
                                wet_buffer[F1D2C(1 , i)] = wet_buffer[F1D2C(1 , i+1)];
        }
        if (count<smartcachesize) {
                                read_pipe(dyn_1_wet_j_k_reader__dyn_1_smart_cache__wet_j_k__pipe,&wet_read_in);
                                wet_buffer[F1D2C(1 , 503)] = wet_read_in;
        }
        if (compindex>=0) {
                                wet_j_k = *wet_buffer;
                                write_pipe(dyn_1_smart_cache__dyn_1__wet_j_k__pipe,&wet_j_k);
                                wet_j_kp1 = wet_buffer[F1D2C(1 , 503)];
                                write_pipe(dyn_1_smart_cache__dyn_1__wet_j_kp1__pipe,&wet_j_kp1);
                                wet_jp1_k = wet_buffer[F1D2C(1 , 2)];
                                write_pipe(dyn_1_smart_cache__dyn_1__wet_jp1_k__pipe,&wet_jp1_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_1() {

      const int nloop = 252004;
      int count;
      float du_j_k;
      float duu;
      float dv_j_k;
      float dvv;
      int j;
      int k;
      float u_j_k;
      float un_j_k;
      float uu;
      float v_j_k;
      float vn_j_k;
      float vv;
      int wet_j_kp1;
      int wet_jp1_k;
      int wet_j_k;
    for (count = 1;count <= nloop;count += 1) {
                read_pipe(dyn_0__dyn_1__du_j_k__pipe,&du_j_k);
                read_pipe(dyn_0__dyn_1__dv_j_k__pipe,&dv_j_k);
                read_pipe(dyn_1_u_j_k_reader__dyn_1__u_j_k__pipe,&u_j_k);
                read_pipe(dyn_1_v_j_k_reader__dyn_1__v_j_k__pipe,&v_j_k);
                read_pipe(dyn_1_smart_cache__dyn_1__wet_j_k__pipe,&wet_j_k);
                read_pipe(dyn_1_smart_cache__dyn_1__wet_j_kp1__pipe,&wet_j_kp1);
                read_pipe(dyn_1_smart_cache__dyn_1__wet_jp1_k__pipe,&wet_jp1_k);
                j = count*1.0/502;
                k = count%502;
        if ((j>=1)&&j<=500&&k>=1&&k<=500) {
                                un_j_k = 0.0;
                                uu = u_j_k;
                                duu = du_j_k;
                if (wet_j_k==1) {
                                if ((wet_j_kp1==1)||duu>0.0) {
                                                                                                                                un_j_k = uu+duu;
                                }
                 } else {
                                if ((wet_j_kp1==1)&&duu<0.0) {
                                                                                                                                un_j_k = uu+duu;
                                }
                }
                                vv = v_j_k;
                                dvv = dv_j_k;
                                vn_j_k = 0.0;
                if (wet_j_k==1) {
                                if ((wet_jp1_k==1)||dvv>0.0) {
                                                                                                                                vn_j_k = vv+dvv;
                                }
                 } else {
                                if ((wet_jp1_k==1)&&dvv<0.0) {
                                                                                                                                vn_j_k = vv+dvv;
                                }
                }
        }
                write_pipe(dyn_1__dyn_2_smart_cache__un_j_k__pipe,&un_j_k);
                write_pipe(dyn_1__dyn_2_smart_cache__vn_j_k__pipe,&vn_j_k);
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_2_h_j_k_reader(float *h) {

      float h_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                h_j_k = h[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(dyn_2_h_j_k_reader__dyn_2_smart_cache__h_j_k__pipe,&h_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_2_eta_j_k_reader(float *eta) {

      float eta_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                eta_j_k = eta[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(dyn_2_eta_j_k_reader__dyn_2__eta_j_k__pipe,&eta_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_2_smart_cache() {

      float h_read_in;
      float un_read_in;
      float vn_read_in;
      float h_buffer[1005];
      float un_buffer[1005];
      float vn_buffer[1005];
      float h_jm1_k;
      float h_j_kp1;
      float h_j_k;
      float h_jp1_k;
      float h_j_km1;
      float un_j_k;
      float un_j_km1;
      float vn_j_k;
      float vn_jm1_k;
      int i;
      int count;
      int compindex;
      const int nloop = 252507;
      const int smartcachesize = 1005;
      const int maxpositiveoffset = 503;
      const int maxnegativeoffset = 503;
    for (count = 1;count <= nloop;count += 1) {
                compindex = count-maxpositiveoffset;

#pragma unroll
        for (i = 1;i <= smartcachesize-1;i += 1) {
                                h_buffer[F1D2C(1 , i)] = h_buffer[F1D2C(1 , i+1)];
                                un_buffer[F1D2C(1 , i)] = un_buffer[F1D2C(1 , 101)];
                                vn_buffer[F1D2C(1 , i)] = vn_buffer[F1D2C(1 , i+1)];
        }
        if (count<smartcachesize) {
                                read_pipe(dyn_2_h_j_k_reader__dyn_2_smart_cache__h_j_k__pipe,&h_read_in);
                                h_buffer[F1D2C(1 , 1005)] = h_read_in;
                                read_pipe(dyn_1__dyn_2_smart_cache__un_j_k__pipe,&un_read_in);
                                un_buffer[F1D2C(1 , 1005)] = un_read_in;
                                read_pipe(dyn_1__dyn_2_smart_cache__vn_j_k__pipe,&vn_read_in);
                                vn_buffer[F1D2C(1 , 1005)] = vn_read_in;
        }
        if (compindex>=0) {
                                h_j_k = h_buffer[F1D2C(1 , 503)];
                                write_pipe(dyn_2_smart_cache__dyn_2__h_j_k__pipe,&h_j_k);
                                h_j_km1 = *h_buffer;
                                write_pipe(dyn_2_smart_cache__dyn_2__h_j_km1__pipe,&h_j_km1);
                                h_j_kp1 = h_buffer[F1D2C(1 , 1005)];
                                write_pipe(dyn_2_smart_cache__dyn_2__h_j_kp1__pipe,&h_j_kp1);
                                h_jm1_k = h_buffer[F1D2C(1 , 502)];
                                write_pipe(dyn_2_smart_cache__dyn_2__h_jm1_k__pipe,&h_jm1_k);
                                h_jp1_k = h_buffer[F1D2C(1 , 504)];
                                write_pipe(dyn_2_smart_cache__dyn_2__h_jp1_k__pipe,&h_jp1_k);
                                un_j_k = un_buffer[F1D2C(1 , 1005)];
                                write_pipe(dyn_2_smart_cache__dyn_2__un_j_k__pipe,&un_j_k);
                                un_j_km1 = un_buffer[F1D2C(1 , 503)];
                                write_pipe(dyn_2_smart_cache__dyn_2__un_j_km1__pipe,&un_j_km1);
                                vn_j_k = vn_buffer[F1D2C(1 , 1005)];
                                write_pipe(dyn_2_smart_cache__dyn_2__vn_j_k__pipe,&vn_j_k);
                                vn_jm1_k = vn_buffer[F1D2C(1 , 1004)];
                                write_pipe(dyn_2_smart_cache__dyn_2__vn_jm1_k__pipe,&vn_jm1_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void dyn_2(float *dt,float *dx,float *dy) {

      const int nloop = 252004;
      int count;
      float eta_j_k;
      float etan_j_k;
      float h_jm1_k;
      float h_j_kp1;
      float h_j_k;
      float h_jp1_k;
      float h_j_km1;
      float hen;
      float hep;
      float hnn;
      float hnp;
      float hsn;
      float hsp;
      float hue;
      float huw;
      float hvn;
      float hvs;
      float hwn;
      float hwp;
      int j;
      int k;
      float un_j_k;
      float un_j_km1;
      float vn_j_k;
      float vn_jm1_k;
    for (count = 1;count <= nloop;count += 1) {
                read_pipe(dyn_2_eta_j_k_reader__dyn_2__eta_j_k__pipe,&eta_j_k);
                read_pipe(dyn_2_smart_cache__dyn_2__h_j_k__pipe,&h_j_k);
                read_pipe(dyn_2_smart_cache__dyn_2__h_j_km1__pipe,&h_j_km1);
                read_pipe(dyn_2_smart_cache__dyn_2__h_j_kp1__pipe,&h_j_kp1);
                read_pipe(dyn_2_smart_cache__dyn_2__h_jm1_k__pipe,&h_jm1_k);
                read_pipe(dyn_2_smart_cache__dyn_2__h_jp1_k__pipe,&h_jp1_k);
                read_pipe(dyn_2_smart_cache__dyn_2__un_j_k__pipe,&un_j_k);
                read_pipe(dyn_2_smart_cache__dyn_2__un_j_km1__pipe,&un_j_km1);
                read_pipe(dyn_2_smart_cache__dyn_2__vn_j_k__pipe,&vn_j_k);
                read_pipe(dyn_2_smart_cache__dyn_2__vn_jm1_k__pipe,&vn_jm1_k);
                j = count*1.0/502;
                k = count%502;
        if ((j>=1)&&j<=500&&k>=1&&k<=500) {
                                hep = 0.5*(un_j_k+(float)fabs(un_j_k))*h_j_k;
                                hen = 0.5*(un_j_k-(float)fabs(un_j_k))*h_j_kp1;
                                hue = hep+hen;
                                hwp = 0.5*(un_j_km1+(float)fabs(un_j_km1))*h_j_km1;
                                hwn = 0.5*(un_j_km1-(float)fabs(un_j_km1))*h_j_k;
                                huw = hwp+hwn;
                                hnp = 0.5*(vn_j_k+(float)fabs(vn_j_k))*h_j_k;
                                hnn = 0.5*(vn_j_k-(float)fabs(vn_j_k))*h_jp1_k;
                                hvn = hnp+hnn;
                                hsp = 0.5*(vn_jm1_k+(float)fabs(vn_jm1_k))*h_jm1_k;
                                hsn = 0.5*(vn_jm1_k-(float)fabs(vn_jm1_k))*h_j_k;
                                hvs = hsp+hsn;
                                etan_j_k = eta_j_k-(*dt)*(hue-huw)*1.0/(*dx)-(*dt)*(hvn-hvs)*1.0/(*dy);
        }
                write_pipe(dyn_2__shapiro_smart_cache__etan_j_k__pipe,&etan_j_k);
                write_pipe(dyn_2__shapiro_smart_cache__un_j_k__pipe,&un_j_k);
                write_pipe(dyn_2__shapiro_smart_cache__vn_j_k__pipe,&vn_j_k);
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void shapiro_wet_j_k_reader(float *wet) {

      float wet_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                wet_j_k = wet[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(shapiro_wet_j_k_reader__shapiro_smart_cache__wet_j_k__pipe,&wet_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void shapiro_smart_cache() {

      float vn_read_in;
      float un_read_in;
      float etan_read_in;
      float wet_read_in;
      float vn_buffer[1005];
      float un_buffer[1005];
      float etan_buffer[1005];
      float wet_buffer[1005];
      float vn_j_k;
      float un_j_k;
      float etan_jm1_k;
      float etan_j_kp1;
      float etan_j_k;
      float etan_jp1_k;
      float etan_j_km1;
      float wet_jm1_k;
      float wet_j_kp1;
      float wet_j_k;
      float wet_jp1_k;
      float wet_j_km1;
      int i;
      int count;
      int compindex;
      const int nloop = 252507;
      const int smartcachesize = 1005;
      const int maxpositiveoffset = 503;
      const int maxnegativeoffset = 503;
    for (count = 1;count <= nloop;count += 1) {
                compindex = count-maxpositiveoffset;

#pragma unroll
        for (i = 1;i <= smartcachesize-1;i += 1) {
                                vn_buffer[F1D2C(1 , i)] = vn_buffer[F1D2C(1 , i+1)];
                                un_buffer[F1D2C(1 , i)] = un_buffer[F1D2C(1 , i+1)];
                                etan_buffer[F1D2C(1 , i)] = etan_buffer[F1D2C(1 , i+1)];
                                wet_buffer[F1D2C(1 , i)] = wet_buffer[F1D2C(1 , i+1)];
        }
        if (count<smartcachesize) {
                                read_pipe(dyn_2__shapiro_smart_cache__etan_j_k__pipe,&etan_read_in);
                                etan_buffer[F1D2C(1 , 1005)] = etan_read_in;
                                read_pipe(dyn_2__shapiro_smart_cache__un_j_k__pipe,&un_read_in);
                                un_buffer[F1D2C(1 , 1005)] = un_read_in;
                                read_pipe(dyn_2__shapiro_smart_cache__vn_j_k__pipe,&vn_read_in);
                                vn_buffer[F1D2C(1 , 1005)] = vn_read_in;
                                read_pipe(shapiro_wet_j_k_reader__shapiro_smart_cache__wet_j_k__pipe,&wet_read_in);
                                wet_buffer[F1D2C(1 , 1005)] = wet_read_in;
        }
        if (compindex>=0) {
                                etan_j_k = etan_buffer[F1D2C(1 , 503)];
                                write_pipe(shapiro_smart_cache__shapiro__etan_j_k__pipe,&etan_j_k);
                                etan_j_km1 = *etan_buffer;
                                write_pipe(shapiro_smart_cache__shapiro__etan_j_km1__pipe,&etan_j_km1);
                                etan_j_kp1 = etan_buffer[F1D2C(1 , 1005)];
                                write_pipe(shapiro_smart_cache__shapiro__etan_j_kp1__pipe,&etan_j_kp1);
                                etan_jm1_k = etan_buffer[F1D2C(1 , 502)];
                                write_pipe(shapiro_smart_cache__shapiro__etan_jm1_k__pipe,&etan_jm1_k);
                                etan_jp1_k = etan_buffer[F1D2C(1 , 504)];
                                write_pipe(shapiro_smart_cache__shapiro__etan_jp1_k__pipe,&etan_jp1_k);
                                un_j_k = un_buffer[F1D2C(1 , 503)];
                                write_pipe(shapiro_smart_cache__shapiro__un_j_k__pipe,&un_j_k);
                                vn_j_k = vn_buffer[F1D2C(1 , 503)];
                                write_pipe(shapiro_smart_cache__shapiro__vn_j_k__pipe,&vn_j_k);
                                wet_j_k = wet_buffer[F1D2C(1 , 503)];
                                write_pipe(shapiro_smart_cache__shapiro__wet_j_k__pipe,&wet_j_k);
                                wet_j_km1 = *wet_buffer;
                                write_pipe(shapiro_smart_cache__shapiro__wet_j_km1__pipe,&wet_j_km1);
                                wet_j_kp1 = wet_buffer[F1D2C(1 , 1005)];
                                write_pipe(shapiro_smart_cache__shapiro__wet_j_kp1__pipe,&wet_j_kp1);
                                wet_jm1_k = wet_buffer[F1D2C(1 , 502)];
                                write_pipe(shapiro_smart_cache__shapiro__wet_jm1_k__pipe,&wet_jm1_k);
                                wet_jp1_k = wet_buffer[F1D2C(1 , 504)];
                                write_pipe(shapiro_smart_cache__shapiro__wet_jp1_k__pipe,&wet_jp1_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void shapiro(float *eps) {

      const int nloop = 252004;
      int count;
      float eta_j_k;
      float etan_jm1_k;
      float etan_j_kp1;
      float etan_j_k;
      float etan_jp1_k;
      float etan_j_km1;
      int j;
      int k;
      float term1;
      float term2;
      float term3;
      int wet_jm1_k;
      int wet_j_kp1;
      int wet_j_k;
      int wet_jp1_k;
      float vn_j_k;
      float un_j_k;
      int wet_j_km1;
    for (count = 1;count <= nloop;count += 1) {
                read_pipe(shapiro_smart_cache__shapiro__etan_j_k__pipe,&etan_j_k);
                read_pipe(shapiro_smart_cache__shapiro__etan_j_km1__pipe,&etan_j_km1);
                read_pipe(shapiro_smart_cache__shapiro__etan_j_kp1__pipe,&etan_j_kp1);
                read_pipe(shapiro_smart_cache__shapiro__etan_jm1_k__pipe,&etan_jm1_k);
                read_pipe(shapiro_smart_cache__shapiro__etan_jp1_k__pipe,&etan_jp1_k);
                read_pipe(shapiro_smart_cache__shapiro__un_j_k__pipe,&un_j_k);
                read_pipe(shapiro_smart_cache__shapiro__vn_j_k__pipe,&vn_j_k);
                read_pipe(shapiro_smart_cache__shapiro__wet_j_k__pipe,&wet_j_k);
                read_pipe(shapiro_smart_cache__shapiro__wet_j_km1__pipe,&wet_j_km1);
                read_pipe(shapiro_smart_cache__shapiro__wet_j_kp1__pipe,&wet_j_kp1);
                read_pipe(shapiro_smart_cache__shapiro__wet_jm1_k__pipe,&wet_jm1_k);
                read_pipe(shapiro_smart_cache__shapiro__wet_jp1_k__pipe,&wet_jp1_k);
                j = count*1.0/502;
                k = count%502;
        if ((j>=1)&&j<=500&&k>=1&&k<=500) {
                if (wet_j_k==1) {
                                                                term1 = (1.0-0.25*(*eps)*(wet_j_kp1+wet_j_km1+wet_jp1_k+wet_jm1_k))*etan_j_k;
                                                                term2 = 0.25*(*eps)*(wet_j_kp1*etan_j_kp1+wet_j_km1*etan_j_km1);
                                                                term3 = 0.25*(*eps)*(wet_jp1_k*etan_jp1_k+wet_jm1_k*etan_jm1_k);
                                                                eta_j_k = term1+term2+term3;
                 } else {
                                                                eta_j_k = etan_j_k;
                }
        }
                write_pipe(shapiro__vernieuw__eta_j_k__pipe,&eta_j_k);
                write_pipe(shapiro__vernieuw__un_j_k__pipe,&un_j_k);
                write_pipe(shapiro__vernieuw__vn_j_k__pipe,&vn_j_k);
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void vernieuw_h_j_k_reader(float *h) {

      float h_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                h_j_k = h[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(vernieuw_h_j_k_reader__vernieuw__h_j_k__pipe,&h_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void vernieuw_hzero_j_k_reader(float *hzero) {

      float hzero_j_k;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                hzero_j_k = hzero[F2D2C(((501 - 0 )+1) , 0,0 , a,b)];
                                write_pipe(vernieuw_hzero_j_k_reader__vernieuw__hzero_j_k__pipe,&hzero_j_k);
        }
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void vernieuw(float hmin) {

      const int nloop = 252004;
      int count;
      float eta_j_k;
      float h_j_k;
      float hzero_j_k;
      int j;
      int k;
      float u_j_k;
      float un_j_k;
      float v_j_k;
      float vn_j_k;
      int wet_j_k;
    for (count = 1;count <= nloop;count += 1) {
                read_pipe(shapiro__vernieuw__eta_j_k__pipe,&eta_j_k);
                read_pipe(vernieuw_h_j_k_reader__vernieuw__h_j_k__pipe,&h_j_k);
                read_pipe(vernieuw_hzero_j_k_reader__vernieuw__hzero_j_k__pipe,&hzero_j_k);
                read_pipe(shapiro__vernieuw__un_j_k__pipe,&un_j_k);
                read_pipe(shapiro__vernieuw__vn_j_k__pipe,&vn_j_k);
                j = count*1.0/502;
                k = count%502;
                h_j_k = hzero_j_k+eta_j_k;
                wet_j_k = 1;
        if (h_j_k<hmin) {
                                wet_j_k = 0;
        }
                u_j_k = un_j_k;
                v_j_k = vn_j_k;
                write_pipe(vernieuw__vernieuw_output_writer__h_j_k__pipe,&h_j_k);
                write_pipe(vernieuw__vernieuw_output_writer__u_j_k__pipe,&u_j_k);
                write_pipe(vernieuw__vernieuw_output_writer__v_j_k__pipe,&v_j_k);
                write_pipe(vernieuw__vernieuw_output_writer__wet_j_k__pipe,&wet_j_k);
    }
    }
__kernel __attribute__((reqd_work_group_size(1,1,1))) void vernieuw_output_writer(float *h,float *u,float *v,float *wet) {

      float h_j_k_read_in;
      float u_j_k_read_in;
      float v_j_k_read_in;
      float wet_j_k_read_in;
      int a;
      int b;
    for (a = 0;a <= 501;a += 1) {
        for (b = 0;b <= 501;b += 1) {
                                read_pipe(vernieuw__vernieuw_output_writer__h_j_k__pipe,&h_j_k_read_in);
                                h[F2D2C(((501 - 0 )+1) , 0,0 , a,b)] = h_j_k_read_in;
                                read_pipe(vernieuw__vernieuw_output_writer__u_j_k__pipe,&u_j_k_read_in);
                                u[F2D2C(((501 - 0 )+1) , 0,0 , a,b)] = u_j_k_read_in;
                                read_pipe(vernieuw__vernieuw_output_writer__v_j_k__pipe,&v_j_k_read_in);
                                v[F2D2C(((501 - 0 )+1) , 0,0 , a,b)] = v_j_k_read_in;
                                read_pipe(vernieuw__vernieuw_output_writer__wet_j_k__pipe,&wet_j_k_read_in);
                                wet[F2D2C(((501 - 0 )+1) , 0,0 , a,b)] = wet_j_k_read_in;
        }
    }
    }
