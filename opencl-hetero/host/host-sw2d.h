#ifndef _HOST_SW2D_H_
#define _HOST_SW2D_H_

//define NX and NY (2 less then the ARRAY-SIZE, ARRAY has additional values for boundry/land)
//TODO: The build script assumes DIM1=DIM2 so just working with it for now
//      But this is an artificial constraint which should be removed eventually
#define NX (ROWS-2)
#define NY (COLS-2)
#define XMID (NX/2)
#define YMID (NY/2)


#include "host-generic.h"
 //this takes care of adding enumerations.h and derivedParameters.h

void sw2d_init_data_host  ( stypeHost *hzero
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
                          );

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
                    );

void sw2d_shapiro_host  ( stypeHost *wet 
                        , stypeHost *etan
                        , stypeHost eps
                        , stypeHost *eta
                        );


void sw2d_updates_host  ( stypeHost *h 
                        , stypeHost *hzero
                        , stypeHost *eta
                        , stypeHost *u
                        , stypeHost *un
                        , stypeHost *v
                        , stypeHost *vn
                        , stypeHost *wet
                        , stypeHost hmin
                        );


#endif
