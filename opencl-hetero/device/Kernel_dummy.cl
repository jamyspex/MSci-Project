void kernel_dyn( const float dt
               , const float dx
               , const float dy
               , const float g
               , __global float * restrict eta
               , __global float * restrict un
               , __global float * restrict u
               , __global float * restrict wet
               , __global float * restrict v
               , __global float * restrict vn
               , __global float * restrict h
               , __global float * restrict etan
               );
void kernel_shapiro ( const float eps
                     , __global float * restrict etan
                     , __global float * restrict wet
                     , __global float * restrict eta
                     );
void kernel_updates ( __global float * restrict h
                    , __global float * restrict hzero
                    , __global float * restrict eta
                    , __global float * restrict u
                    , __global float * restrict un
                    , __global float * restrict v
                    , __global float * restrict vn
                    , __global float * restrict wet
                    , float hmin
                    );
__kernel void Kernel( const float dt
                    , const float dx
                    , const float dy
                    , const float g
                    , const float eps
                    , const float hmin
                    , __global float * restrict eta
                    , __global float * restrict un
                    , __global float * restrict u
                    , __global float * restrict wet
                    , __global float * restrict v
                    , __global float * restrict vn
                    , __global float * restrict h
                    , __global float * restrict etan
                    , __global float * restrict hzero
                    ) {
/*
kernel_dyn( dt
          , dx
          , dy
          , g
          , eta
          , un
          , u
          , wet
          , v
          , vn
          , h
          , etan
          );
kernel_shapiro ( eps
                , etan
                , wet
                , eta
                );
kernel_updates ( h
               , hzero
               , eta
               , u
               , un
               , v
               , vn
               , wet
               , hmin
               );
*/
}
void kernel_dyn( const float dt
               , const float dx
               , const float dy
               , const float g
               , __global float * restrict eta
               , __global float * restrict un
               , __global float * restrict u
               , __global float * restrict wet
               , __global float * restrict v
               , __global float * restrict vn
               , __global float * restrict h
               , __global float * restrict etan
               ) {
float du;
float dv;
float uu;
float vv;
float duu;
float dvv;
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
int j, k;
  for (j=1; j<= 32 -2; j++) {
    for (k=1; k<= 32 -2; k++) {
      duu = -dt
           * g
           * ( eta[j*32 + k+1]
             - eta[j*32 + k ]
             )
           / dx;
      dvv = -dt
           * g
           * ( eta[(j+1)*32 + k]
             - eta[ j*32 + k]
             )
           / dy;
      un[j*32 + k] = 0.0;
      uu = u[j*32 + k];
      if ( ( (wet[j*32 + k] == 1)
              && ( (wet[j*32 + k+1] == 1) || (duu > 0.0)))
         || ( (wet[j*32 + k+1] == 1) && (duu < 0.0))
         ){
          un[j*32 + k] = uu+duu;
      }
      vn[j*32 + k] = 0.0;
      vv = v[j*32 + k];
      if ( ( (wet[j*32 + k] == 1)
             && ( (wet[j*32 + k+1] == 1) || (dvv > 0.0)))
         || ((wet[j*32 + k+1] == 1) && (dvv < 0.0))
         ){
          vn[j*32 + k] = vv+dvv;
      }
    }
  }
  for (j=1; j<= 32 -2; j++) {
    for (k=1; k<= 32 -2; k++) {
      hep = 0.5*( un[j*32 + k] + fabs(un[j*32 + k]) ) * h[j*32 + k ];
      hen = 0.5*( un[j*32 + k] - fabs(un[j*32 + k]) ) * h[j*32 + k+1];
      hue = hep+hen;
      hwp = 0.5*( un[j*32 + k-1] + fabs(un[j*32 + k-1]) ) * h[j*32 + k-1];
      hwn = 0.5*( un[j*32 + k-1] - fabs(un[j*32 + k-1]) ) * h[j*32 + k ];
      huw = hwp+hwn;
      hnp = 0.5*( vn[j*32 + k] + fabs(vn[j*32 + k]) ) * h[ j*32 + k];
      hnn = 0.5*( vn[j*32 + k] - fabs(vn[j*32 + k]) ) * h[(j+1)*32 + k];
      hvn = hnp+hnn;
      hsp = 0.5*( vn[(j-1)*32 + k] + fabs(vn[(j-1)*32 + k]) ) * h[(j-1)*32 + k];
      hsn = 0.5*( vn[(j-1)*32 + k] - fabs(vn[(j-1)*32 + k]) ) * h[ j*32 + k];
      hvs = hsp+hsn;
      etan[j*32 + k] = eta[j*32 + k]
                        - dt*(hue-huw)/dx
                        - dt*(hvn-hvs)/dy;
    }
  }
}
void kernel_shapiro ( const float eps
                        , __global float * restrict etan
                        , __global float * restrict wet
                        , __global float * restrict eta
                        ) {
  int j,k;
  float term1,term2,term3;
  for (j=1; j<= 32 -2; j++) {
    for (k=1; k<= 32 -2; k++) {
        if (wet[j*32 + k]==1) {
        term1 = ( 1.0-0.25*eps
                  * ( wet[ j*32 + k+1]
                    + wet[ j*32 + k-1]
                    + wet[(j+1)*32 + k ]
                    + wet[(j-1)*32 + k ]
                    )
                )
                * etan[j*32 + k];
        term2 = 0.25*eps
                * ( wet [j*32 + k+1]
                  * etan[j*32 + k+1]
                  + wet [j*32 + k-1]
                  * etan[j*32 + k-1]
                  );
        term3 = 0.25*eps
                * ( wet [(j+1)*32 + k]
                  * etan[(j+1)*32 + k]
                  + wet [(j-1)*32 + k]
                  * etan[(j-1)*32 + k]
                  );
        eta[(j-1)*32 + k] = term1 + term2 + term3;
      }
      else {
        eta[(j-1)*32 + k] = etan[(j-1)*32 + k];
      }
    }
  }
}
void kernel_updates ( __global float * restrict h
                    , __global float * restrict hzero
                    , __global float * restrict eta
                    , __global float * restrict u
                    , __global float * restrict un
                    , __global float * restrict v
                    , __global float * restrict vn
                    , __global float * restrict wet
                    , float hmin
                    ) {
  for (int j=0; j<= 32 -1; j++) {
    for (int k=0; k<=32 -1; k++) {
      h[j*32 + k] = hzero[j*32 + k]
                    + eta [j*32 + k];
      wet[j*32 + k] = 1;
      if ( h[j*32 + k] < hmin )
            wet[j*32 + k] = 0;
      u[j*32 + k] = un[j*32 + k];
      v[j*32 + k] = vn[j*32 + k];
    }
  }
}
