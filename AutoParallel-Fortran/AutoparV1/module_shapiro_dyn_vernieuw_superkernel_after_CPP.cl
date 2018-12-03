inline unsigned int F3D2C(
        unsigned int i_rng,unsigned int j_rng,
        int i_lb, int j_lb, int k_lb,
        int ix, int jx, int kx
        ) {
    return (i_rng*j_rng*(kx-k_lb)+i_rng*(jx-j_lb)+ix-i_lb);
}
inline unsigned int F2D2C(
        unsigned int i_rng,
        int i_lb, int j_lb,
        int ix, int jx
        ) {
    return (i_rng*(jx-j_lb)+ix-i_lb);
}
inline unsigned int F1D2C(
        int i_lb,
        int ix
        ) {
    return ix-i_lb;
}
inline unsigned int F4D2C(
        unsigned int i_rng,unsigned int j_rng, unsigned int k_rng,
        int i_lb, int j_lb, int k_lb, int l_lb,
        int ix, int jx, int kx, int lx
        ) {
    return (i_rng*j_rng*k_rng*(lx-l_lb)+
            i_rng*j_rng*(kx-k_lb)+
            i_rng*(jx-j_lb)+
            ix-i_lb
            );
}
void shapiro_map_15(__global int *wet,__global float *eps,__global float *etan,__global float *eta) {
          const int nx = 500;
          const int ny = 500;
    int j;
    int k;
    float term1;
    float term2;
    float term3;
    int j_range;
    int k_range;
    int j_rel;
    int k_rel;
    int global_id;
        global_id = get_global_id(0);
        j_range = ((500-1)+1);
        k_range = ((500-1)+1);
        j_rel = global_id*1.0/k_range;
        j = j_rel+1;
        k_rel = (global_id-(j_rel*k_range));
        k = k_rel+1;
    if (wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]==1) {
    term1 = (1.0-0.25*(*eps)*(wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)]+wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]+wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)]+wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]))*etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    term2 = 0.25*(*eps)*(wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)]*etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)]+wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]*etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]);
    term3 = 0.25*(*eps)*(wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)]*etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)]+wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]*etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]);
    eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = term1+term2+term3;
   } else {
    eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
  }
  }
void dyn_map_38(__global float *dt,__global float *g,__global float *eta,__global float *dx,__global float *dy,__global float *u,__global float *du,__global int *wet,__global float *duu,__global float *v,__global float *dv,__global float *dvv,__global float *un,__global float *h,__global float *vn,__global float *etan) {
          const int ny = 500;
          const int nx = 500;
    int j;
    int k;
    float uu;
    float vv;
    float hep;
    float hen;
    float hue;
    float hwp;
    float hwn;
    float huw;
    float hnp;
    float hnn;
    float hvn;
    float hsp;
    float hsn;
    float hvs;
    int j_range;
    int k_range;
    int j_rel;
    int k_rel;
    int global_id;
        global_id = get_global_id(0);
        j_range = ((500-1)+1);
        k_range = ((500-1)+1);
        j_rel = global_id*1.0/k_range;
        j = j_rel+1;
        k_rel = (global_id-(j_rel*k_range));
        k = k_rel+1;
    du[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = -(*dt)*(*g)*(eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)]-eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)])*1.0/(*dx);
    dv[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = -(*dt)*(*g)*(eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)]-eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)])*1.0/(*dy);
    un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = 0.0;
    uu = u[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    *duu = du[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
  if (wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]==1) {
  if ((wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)]==1)||(*duu)>0.0) un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = uu+(*duu);
   } else {
  if ((wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)]==1)&&(*duu)<0.0) un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = uu+(*duu);
  }
    vv = v[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    *dvv = dv[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = 0.0;
  if (wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]==1) {
  if ((wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)]==1)||(*dvv)>0.0) vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = vv+(*dvv);
   } else {
  if ((wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)]==1)&&(*dvv)<0.0) vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = vv+(*dvv);
  }
    hep = 0.5*(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]+(float)fabs(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    hen = 0.5*(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]-(float)fabs(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k+1)];
    hue = hep+hen;
    hwp = 0.5*(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]+(float)fabs(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)];
    hwn = 0.5*(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]-(float)fabs(un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k-1)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    huw = hwp+hwn;
    hnp = 0.5*(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]+(float)fabs(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    hnn = 0.5*(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]-(float)fabs(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j+1,k)];
    hvn = hnp+hnn;
    hsp = 0.5*(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]+(float)fabs(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)];
    hsn = 0.5*(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]-(float)fabs(vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j-1,k)]))*h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    hvs = hsp+hsn;
    etan[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]-(*dt)*(hue-huw)*1.0/(*dx)-(*dt)*(hvn-hvs)*1.0/(*dy);
  }
void vernieuw_map_23(__global float *hzero,__global float *eta,__global float *h,__global float *hmin,__global float *un,__global float *vn,__global int *wet,__global float *u,__global float *v) {
    const int nx = 500;
    const int ny = 500;
    int j;
    int k;
    int j_range;
    int k_range;
    int j_rel;
    int k_rel;
    int global_id;
        global_id = get_global_id(0);
        j_range = (((500+1)-0)+1);
        k_range = (((500+1)-0)+1);
        j_rel = global_id*1.0/k_range;
        j = j_rel+0;
        k_rel = (global_id-(j_rel*k_range));
        k = k_rel+0;
    h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = hzero[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]+eta[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = 1;
  if (h[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)]<(*hmin)) wet[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = 0;
    u[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = un[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
    v[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)] = vn[F2D2C((((ny+1) - 0 )+1) , 0,0 , j,k)];
  }
__kernel void shapiro_dyn_vernieuw_superkernel(__global int *wet,__global float *eps,__global float *etan,__global float *eta,__global float *dt,__global float *g,__global float *dx,__global float *dy,__global float *u,__global float *du,__global float *duu,__global float *v,__global float *dv,__global float *dvv,__global float *un,__global float *h,__global float *vn,__global float *hzero,__global float *hmin,__global int *state_ptr) {
  int *j;
  int state;
  const int st_shapiro_map_15 = 0;
  const int st_dyn_map_38 = 1;
  const int st_vernieuw_map_23 = 2;
    state = *state_ptr;
  switch ( state ) {
        case (st_shapiro_map_15): {
            shapiro_map_15(wet,eps,etan,eta);
        } break;
        case (st_dyn_map_38): {
            dyn_map_38(dt,g,eta,dx,dy,u,du,wet,duu,v,dv,dvv,un,h,vn,etan);
        } break;
        case (st_vernieuw_map_23): {
            vernieuw_map_23(hzero,eta,h,hmin,un,vn,wet,u,v);
      }
  }
  }
