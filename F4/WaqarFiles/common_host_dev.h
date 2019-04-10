//WN, Glasgow, 2019.04.08
//This are the parameters that must be kept common  across host and device code.
// ** NOTE ** that if you make changes to it, make will recompile host but _not_
// the device. So you must run make clean and then make again to ensure device 
// recompiles with these parameters

// ----------------------------------------
// 2d shallow water model specific params
// ----------------------------------------
#define _ROWS       502
#define _COLS       ROWS
#define _NX         (ROWS-2)
#define _NY         (COLS-2)
#define _XMID       (NX/2)
#define _YMID       (NY/2)
#define _DATA_SIZE  (ROWS*COLS)
#define _NTOT       1
  //how many time steps?
//#define NOUT 5
  //log output after every how many steps?

typedef float data_t;
  // the type of data for the simulation    


