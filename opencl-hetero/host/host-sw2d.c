#include "host-sw2d.h"


//**NOTE**
// ROWS = STREAM_ARRAY_SIZE_DIM1 = NX+2
// COLS = STREAM_ARRAY_SIZE_DIM2 = NY+2
  //(array size is +1 on each side of NX and NY for land boundary)
// Linear index version of 2D: a[x][y] = a[x*COLS + y]


//------------------------------------------
// initialize 2D shallow-water host arrays
//------------------------------------------
void sw2d_init_data_host ( stypeHost *hzero
                    , stypeHost *eta  
                    , stypeHost *etan 
                    , stypeHost *h    
                    , stypeHost *wet  
                    , stypeHost *u    
                    , stypeHost *un   
                    , stypeHost *v    
                    , stypeHost *vn
                    , stypeHost hmin
                    , int BytesPerWord
                    ) {
      
//FILE * fdebug;
//fdebug= fopen ("debug.dat","w");

  int j, k;
  //initialize height
  for (j=0; j<=ROWS-1; j++) {
    for (k=0; k<=COLS-1; k++) {
      hzero[j*COLS + k] = 10.0;
    }
  }

  //land boundaries with 10 m elevation
  for (k=0; k<=COLS-1; k++) {
    //top-row
    hzero[0*COLS + k] = -10.0;
    //bottom-row (ROWS-1)
    hzero[(ROWS-1)*COLS + k] = -10.0;
  }
  for (j=0; j<=ROWS-1; j++) {
    //left-most-col
    hzero[j*COLS + 0] = -10.0;
    //right-most-col
    hzero[j*COLS + COLS-1] = -10.0;
  }

  // eta and etan
  for (j=0; j<= ROWS-1; j++) {
    for (k=0; k<=COLS-1; k++) {
      eta [j*COLS + k] = -MIN(0.0, hzero[j*COLS + k] );
      etan[j*COLS + k] = eta[j*COLS + k];
//      fprintf(fdebug, "j = %d, k = %d, eta = %f\n"
//                       ,j, k, eta[j*COLS + k]);
    }                                                                           
  } 

  //h, wet, u, un, v, vn
  // eta and etan
  for (j=0; j<= ROWS-1; j++) {
    for (k=0; k<= COLS-1; k++) {
      //h
      h[j*COLS + k] = hzero[j*COLS + k] 
                    +   eta[j*COLS + k];
      //wet                   
      //wet = 1 defines "wet" grid cells 
      //wet = 0 defines "dry" grid cells (land)
//temp-for-debug
//    wet[j*COLS + k] = j*COLS + k +1; 

      
//#if 0
      wet[j*COLS + k] = 1; 
      if (h[j*COLS + k] < hmin)
       wet[j*COLS + k] = 0; 
//#endif
      //u, v, un, vn
      u [j*COLS + k] = 0;
      un[j*COLS + k] = 0;
      v [j*COLS + k] = 0;
      vn[j*COLS + k] = 0;

//printf("HOST-INIT:j = %d, k = %d,  wet = %f\n"
//               , j, k, wet[j*COLS + k]
//      );

    }
  }

  //Initial Condition... Give eta=1 @ MID_POINT
  //-------------------------------------------
  *(eta+ XMID*COLS + YMID) = 1.0;


 printf("Host arrays initialized.\n");
 printf(HLINE);

 //fclose(fdebug);  
}

//------------------------------------------
// dyn() - the dynamics
//------------------------------------------

void sw2d_dyn_host  ( stypeHost dt
                    , stypeHost dx
                    , stypeHost dy
                    , stypeHost g
                    , stypeHost *eta
                    , stypeHost *un
                    , stypeHost *u
                    , stypeHost *wet
                    , stypeHost *v
                    , stypeHost *vn
                    , stypeHost *h
                    , stypeHost *etan
                    , int BytesPerWord
                    ) {



//locals
//-------------------
stypeHost *du;// [ROWS][COLS];
stypeHost *dv;// [ROWS][COLS];
posix_memalign ((void**)&du, ALIGNMENT, SIZE*BytesPerWord);
posix_memalign ((void**)&dv, ALIGNMENT, SIZE*BytesPerWord);
stypeHost uu;
stypeHost vv;
stypeHost duu;
stypeHost dvv;
stypeHost hue;
stypeHost huw;
stypeHost hwp;
stypeHost hwn;
stypeHost hen;
stypeHost hep;
stypeHost hvn;
stypeHost hvs;
stypeHost hsp;
stypeHost hsn;
stypeHost hnn;
stypeHost hnp;
int j, k;

//FILE * fdebug;
//fdebug= fopen ("debug.dat","w");

//what's eta?
//-------------------------------------------
//  for (j=0; j<= ROWS-1; j++) {
//    for (k=0; k<= COLS-1; k++) {
//      printf("j = %d, k = %d, eta =  %f\n"
//                       ,j, k, eta[j*COLS + k]);
//    }
//  }



//calculate du, dv on all non-boundary points
//-------------------------------------------
  for (j=1; j<= ROWS-2; j++) {
    for (k=1; k<= COLS-2; k++) {





      du[j*COLS + k]  = -dt 
                      * g
                      * ( eta[j*COLS + k+1 ]
                        - eta[j*COLS + k   ]
                        ) 
                      / dx;
      dv[j*COLS + k]  = -dt 
                      * g
                      * ( eta[(j+1)*COLS + k]
                        - eta[    j*COLS + k]
                        ) 
                      / dy;
//      fprintf(fdebug, "j = %d, k = %d, du = %f, dv = %f, eta[j*COLS + k+1 ] = %f, eta[(j+1)*COLS + k] = %f, eta[j*COLS + k   ] = %f\n"
//                       ,j, k, du[j*COLS + k], dv[j*COLS + k], eta[j*COLS + k+1 ], eta[(j+1)*COLS + k], eta[j*COLS + k   ]);
    }
  }


//prediction for u and v
//---------------------------------
  for (j=1; j<= ROWS-2; j++) {
    for (k=1; k<= COLS-2; k++) {

      //u
      un[j*COLS + k]  = 0.0;
      uu = u[j*COLS + k];
      duu= du[j*COLS + k];
      if (wet[j*COLS + k] == 1){
        if( (wet[j*COLS + k+1] == 1) || (duu > 0.0) ) 
          un[j*COLS + k] = uu+duu;
      }//if
      else {
        if((wet[j*COLS + k+1] == 1) && (duu < 0.0) )
          un[j*COLS + k] = uu+duu;
      }//else

      //v
      vn[j*COLS + k]  = 0.0;
      vv =  v[j*COLS + k];
      dvv= dv[j*COLS + k];
      if (wet[j*COLS + k] == 1){
        if( (wet[(j+1)*COLS + k] == 1) || (dvv > 0.0) ) 
          vn[j*COLS + k] = vv+dvv;
      }//if
      else {
        if((wet[(j+1)*COLS + k] == 1) && (dvv < 0.0) )
          vn[j*COLS + k] = vv+dvv;
      }//else

//    fprintf(fdebug, "j = %d, k = %d, un = %f, vn = %f, wet = %f, wet_j+1 = %f,  uu = %f, duu = %f, vv = %f, dvv = %f \n"
//                   ,j, k, un[j*COLS + k], vn[j*COLS + k], wet[j*COLS + k], wet[(j+1)*COLS + k], uu, duu, vv, dvv
//    printf("HOST-DYN1:j = %d, k = %d, un = %f, vn = %f, wet = %f, wet_jp1_k = %f, wet_j_kp1 = %f,  eta_j_k = %f, eta_jp1_k = %f, eta_j_kp1 = %f, uu = %f, duu = %f, vv = %f, dvv = %f \n"
//                   ,j, k, un[j*COLS + k], vn[j*COLS + k]
//                   , wet[j*COLS + k], wet[(j+1)*COLS + k], wet[j*COLS + k+1]
//                   , eta[j*COLS + k], eta[(j+1)*COLS + k], eta[j*COLS + k+1]
//                   , uu, duu, vv, dvv
//          );
    }//for
  }//for

//sea level predictor
//--------------------
  for (j=1; j<= ROWS-2; j++) {
    for (k=1; k<= COLS-2; k++) {   
      hep = 0.5*( un[j*COLS + k] + fabs(un[j*COLS + k]) ) * h[j*COLS + k  ];
      hen = 0.5*( un[j*COLS + k] - fabs(un[j*COLS + k]) ) * h[j*COLS + k+1];
      hue = hep+hen;

      hwp = 0.5*( un[j*COLS + k-1] + fabs(un[j*COLS + k-1]) ) * h[j*COLS + k-1];
      hwn = 0.5*( un[j*COLS + k-1] - fabs(un[j*COLS + k-1]) ) * h[j*COLS + k  ];
      huw = hwp+hwn;

      hnp = 0.5*( vn[j*COLS + k] + fabs(vn[j*COLS + k]) ) * h[    j*COLS + k];
      hnn = 0.5*( vn[j*COLS + k] - fabs(vn[j*COLS + k]) ) * h[(j+1)*COLS + k];
      hvn = hnp+hnn;

      hsp = 0.5*( vn[(j-1)*COLS + k] + fabs(vn[(j-1)*COLS + k]) ) * h[(j-1)*COLS + k];
      hsn = 0.5*( vn[(j-1)*COLS + k] - fabs(vn[(j-1)*COLS + k]) ) * h[    j*COLS + k];
      hvs = hsp+hsn;

      etan[j*COLS + k]  = eta[j*COLS + k]
                        - dt*(hue-huw)/dx
                        - dt*(hvn-hvs)/dy;
//    fprintf(fdebug, "j = %d, k = %d, etan = %f, eta = %f \n"
//                    ,j, k, etan[j*COLS + k], eta[j*COLS + k]
//            );
//    printf("HOST-DYN2::j = %d, k = %d, etan = %f, eta = %f \n"
//                    ,j, k, etan[j*COLS + k], eta[j*COLS + k]
//          );
    }//for
  }//for  

//fclose(fdebug);  
}//()

//------------------------------------------
// shapiro() - filter
//------------------------------------------
void sw2d_shapiro_host  ( stypeHost *wet 
                        , stypeHost *etan
                        , stypeHost eps
                        , stypeHost *eta
                        ) {

  //locals
  int j,k;
  stypeHost term1,term2,term3;

  //1-order Shapiro filter
  for (j=1; j<= ROWS-2; j++) {
    for (k=1; k<= COLS-2; k++) {   
        if (wet[j*COLS + k]==1) {
        term1 = ( 1.0-0.25*eps
                  * ( wet[    j*COLS + k+1] 
                    + wet[    j*COLS + k-1] 
                    + wet[(j+1)*COLS + k  ] 
                    + wet[(j-1)*COLS + k  ] 
                    ) 
                )
                * etan[j*COLS + k]; 
        term2 = 0.25*eps
                * ( wet [j*COLS + k+1]
                  * etan[j*COLS + k+1]
                  + wet [j*COLS + k-1]
                  * etan[j*COLS + k-1]
                  );
        term3 = 0.25*eps
                * ( wet [(j+1)*COLS + k]
                  * etan[(j+1)*COLS + k]
                  + wet [(j-1)*COLS + k]
                  * etan[(j-1)*COLS + k]
                  );
        eta[j*COLS + k] = term1 + term2 + term3;
      }//if
      else {
        eta[j*COLS + k] = etan[j*COLS + k];
      }//else

//      printf("HOST\t:j = %d, k = %d, eta = %f, wet_j_k = %f, wet_jp1_k = %f, wet_j_kp1 = %f, wet_jm1_k = %f, wet_j_km1 = %f \n"
//                      ,j, k, eta[j*COLS + k]
//                      ,wet[j*COLS + k] 
//                      ,wet[(j+1)*COLS + k] 
//                      ,wet[j*COLS + k+1] 
//                      ,wet[(j-1)*COLS + k] 
//                      ,wet[j*COLS + k-1] 
//            );
    }//for
  }//for
}//()

//------------------------------------------
// updates() - 
// in the original this was part of main
//------------------------------------------
void sw2d_updates_host  ( stypeHost *h 
                        , stypeHost *hzero
                        , stypeHost *eta
                        , stypeHost *u
                        , stypeHost *un
                        , stypeHost *v
                        , stypeHost *vn
                        , stypeHost *wet
                        , stypeHost hmin
                        ) {

  for (int j=0; j<= ROWS-1; j++) {
    for (int k=0; k<=COLS-1; k++) {
      //h update
      h[j*COLS + k] = hzero[j*COLS + k] 
                    + eta  [j*COLS + k];
      //wet update
      wet[j*COLS + k] = 1;
      if ( h[j*COLS + k] < hmin )
            wet[j*COLS + k] = 0;
      //u, v updates
      u[j*COLS + k] = un[j*COLS + k];
      v[j*COLS + k] = vn[j*COLS + k];

//      printf("HOST\t:computed: j = %d, k = %d, h = %f,h_zero = %f, eta_j_k = %f \n"
//                      ,j, k
//                      ,h[j*COLS + k] 
//                      ,hzero[j*COLS + k] 
//                      ,eta[j*COLS + k] 
//            );
    }//for
  }//for
}//()
