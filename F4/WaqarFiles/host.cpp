/**********
Copyright (c) 2018, Xilinx, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors
may be used to endorse or promote products derived from this software
without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**********/

// **********************************************************************
//
//WN: Glasgow, 20190403, 
// 2d-shallow water host
// Based on Sdx provided examples, helloworld_ocl, and dataflow_pipes_ocl
// **********************************************************************

#include "xcl2.hpp"
#include <vector>
#include <cmath>
#include <ctime>

//#define VERBOSE

using namespace std;
//using std::vector;
static const std::string error_message =
    "Error: Result mismatch:\n"
    "i = %d CPU result = %d Device result = %d\n";

// ----------------------------------------
// Kernel source file and kernel names(s)
// ----------------------------------------
// Select ONE of the following three kernel target options
// or define one of them via command line/Make
//#define USE_SINGLE_KERNEL
//#define USE_KERNEL_PIPES_MANUAL    
//#define USE_KERNEL_PIPES_AUTO

#ifdef USE_SINGLE_KERNEL
//----------------------
  #define NKERNELS 1
  #define KERNEL_SRC_FILE_NO_EXT "Kernels_singleKernel"
  
  const char* kernel_names[NKERNELS] = 
    {
       "Kernel"
    };
  const char* kernelVersionMessage = "Single Kernel";
#endif    

#ifdef USE_KERNEL_PIPES_MANUAL
//-----------------------------
  #define KERNEL_SRC_FILE_NO_EXT "Kernels_withpipes_manual"
  #define NKERNELS                9
  #define K_MEM_RD                0
  #define K_SMACHE_MEMRD_2_DYN1   1
  #define K_DYN1                  2
  #define K_SMACHE_DYN1_2_DYN2    3
  #define K_DYN2                  4
  #define K_SMACHE_DYN2_2_SHAPIRO 5
  #define K_SHAPIRO               6
  #define K_UPDATES               7
  #define K_MEM_WR                8    

  const char* kernel_names[NKERNELS] = 
    {
       "kernel_mem_rd"
      ,"kernel_smache_memrd_2_dyn1"
      ,"kernel_dyn1"
      ,"kernel_smache_dyn1_2_dyn2"
      ,"kernel_dyn2"
      ,"kernel_smache_dyn2_2_shapiro"
      ,"kernel_shapiro"
      ,"kernel_updates"
      ,"kernel_mem_wr"
    };
  const char* kernelVersionMessage = "Multiple Kernels with Pipes - Manual";
#endif    


#ifdef USE_KERNEL_PIPES_AUTO
//-----------------------------
  #define KERNEL_SRC_FILE_NO_EXT "Kernels_withpipes_auto"
  #define NKERNELS                    19
  #define K_dyn_0_eta_j_k_reader      0
  #define K_dyn_0_smart_cache         1
  #define K_dyn_0                     2
  #define K_dyn_1_wet_j_k_reader      3
  #define K_dyn_1_u_j_k_reader        4
  #define K_dyn_1_v_j_k_reader        5
  #define K_dyn_1_smart_cache         6
  #define K_dyn_1                     7
  #define K_dyn_2_h_j_k_reader        8   
  #define K_dyn_2_eta_j_k_reader      9
  #define K_dyn_2_smart_cache         10
  #define K_dyn_2                     11
  #define K_shapiro_wet_j_k_reader    12
  #define K_shapiro_smart_cache       13
  #define K_shapiro                   14
  #define K_vernieuw_h_j_k_reader     15
  #define K_vernieuw_hzero_j_k_reader 16
  #define K_vernieuw                  17
  #define K_vernieuw_output_writer    18
  
  const char* kernel_names[NKERNELS] = 
    {
       "dyn_0_eta_j_k_reader"
      ,"dyn_0_smart_cache"
      ,"dyn_0"
      ,"dyn_1_wet_j_k_reader"
      ,"dyn_1_u_j_k_reader"
      ,"dyn_1_v_j_k_reader"
      ,"dyn_1_smart_cache"
      ,"dyn_1"
      ,"dyn_2_h_j_k_reader"
      ,"dyn_2_eta_j_k_reader"
      ,"dyn_2_smart_cache"
      ,"dyn_2"
      ,"shapiro_wet_j_k_reader"
      ,"shapiro_smart_cache"
      ,"shapiro"
      ,"vernieuw_h_j_k_reader"
      ,"vernieuw_hzero_j_k_reader"
      ,"vernieuw"
      ,"vernieuw_output_writer"
    };
  const char* kernelVersionMessage = "Multiple Kernels with Pipes - Auto";
#endif 


// ----------------------------------------
// Convenience Macros
// ----------------------------------------
# define HLINE "-------------------------------------------------------------\n"
# define SLINE "*************************************************************\n"

# ifndef MIN
# define MIN(x,y) ((x)<(y)?(x):(y))
# endif
# ifndef MAX
# define MAX(x,y) ((x)>(y)?(x):(y))
# endif
    

// ----------------------------------------
// 2d shallow water model specific params
// ----------------------------------------
#include "common_host_dev.h"    

const int ROWS      = _ROWS     ;
const int COLS      = _COLS     ;
const int NX        = _NX       ;
const int NY        = _NY       ;
const int XMID      = _XMID     ;
const int YMID      = _YMID     ;
const int DATA_SIZE = _DATA_SIZE;
const int NTOT      = _NTOT     ;

#define EPSILON 0.0001
  // tolerance when comparing CPU vs DEV floating point results 

// ----------------------------------------
//SDx specific params
// ----------------------------------------
#define SDACCEL_ALIGNMENT 64
#define ALIGNMENT SDACCEL_ALIGNMENT

// ----------------------------------------
// 2d shallow water specific function signs
// ----------------------------------------
void sw2d_init_data_host ( data_t *hzero
                         , data_t *eta  
                         , data_t *etan 
                         , data_t *h    
                         , data_t *wet  
                         , data_t *u    
                         , data_t *un   
                         , data_t *v    
                         , data_t *vn
                         , data_t hmin
                         , int    bytes_per_word
                         );

void sw2d_dyn_host  ( data_t dt
                    , data_t dx
                    , data_t dy
                    , data_t g                    
                    , data_t *eta
                    , data_t *un
                    , data_t *u
                    , data_t *wet
                    , data_t *v
                    , data_t *vn
                    , data_t *h
                    , data_t *etan
                    , int bytes_per_word                    
                    );

void sw2d_shapiro_host  ( data_t *wet 
                        , data_t *etan
                        , data_t eps
                        , data_t *eta
                        );


void sw2d_updates_host  ( data_t *h 
                        , data_t *hzero
                        , data_t *eta
                        , data_t *u
                        , data_t *un
                        , data_t *v
                        , data_t *vn
                        , data_t *wet
                        , data_t hmin
                        );


int verify_results ( float* h
                   , float* h_g
                   );

// ----------------------------------------
int main(int argc, char **argv) {
// ----------------------------------------

  //-------------------------------------------------------------------------------------------
  //Initialization
  //-------------------------------------------------------------------------------------------  


  // compute the size of array in bytes
    size_t size_in_bytes  = DATA_SIZE * sizeof(int);
    size_t bytes_per_word = sizeof(float);
    int ntot = NTOT; //how many time steps
    int nout = 5;    //log output after every how many steps?
    int rows = ROWS;
    int cols = COLS;

    cout << HLINE << "*Simulation Parameters*\n";  
    cout << "Kernel Version = " << kernelVersionMessage << endl;
    cout << "ROWS           = " << ROWS           << endl;
    cout << "COLS           = " << COLS           << endl;
    cout << "NX             = " << NX             << endl;
    cout << "NY             = " << NY             << endl;
    cout << "XMID           = " << XMID           << endl;
    cout << "YMID           = " << YMID           << endl;
    cout << "DATA_SIZE      = " << DATA_SIZE      << endl;
    cout << "NTOT           = " << NTOT           << endl;
    cout << "size_in_bytes  = " << size_in_bytes  << endl;
    cout << "bytes_per_word = " << bytes_per_word << endl;

    std::cout << HLINE << "CPU: Initializing data\n";  
    //simulation related scalars   
    data_t hmin = 0.05;
    data_t dx = 10.0;
    data_t dy = 10.0;
    data_t dt = 0.1;
    data_t g = 9.81;  
    data_t eps = 0.05;
    data_t hmin_g = 0.05; //golden copies of scalars not really needed, but to avoid bugs
    data_t dx_g = 10.0;
    data_t dy_g = 10.0;
    data_t dt_g = 0.1;
    data_t g_g = 9.81;  
    data_t eps_g = 0.05;

    // host arrays ("golden" arrays, computed during host run, used for verification)
    vector<data_t,aligned_allocator<data_t>> eta_g  (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> etan_g (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> h_g    (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> hzero_g(DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> wet_g  (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> u_g    (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> un_g   (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> v_g    (DATA_SIZE);
    vector<data_t,aligned_allocator<data_t>> vn_g   (DATA_SIZE);

    //create pointers for using legacy C functions
    data_t *p_eta_g   = &eta_g  [0];
    data_t *p_etan_g  = &etan_g [0];
    data_t *p_h_g     = &h_g    [0];
    data_t *p_hzero_g = &hzero_g[0];
    data_t *p_wet_g   = &wet_g  [0];
    data_t *p_u_g     = &u_g    [0];
    data_t *p_un_g    = &un_g   [0];
    data_t *p_v_g     = &v_g    [0];
    data_t *p_vn_g    = &vn_g   [0];


    //initialize arrays (using legacy function from C, so need to pass pointer to first element of vector)
    sw2d_init_data_host ( p_hzero_g
                        , p_eta_g
                        , p_etan_g
                        , p_h_g
                        , p_wet_g
                        , p_u_g
                        , p_un_g
                        , p_v_g
                        , p_vn_g
                        , hmin
                        , bytes_per_word
                        );

  //-------------------------------------------------------------------------------------------
  //HOST RUN
  //-------------------------------------------------------------------------------------------
  std::cout << HLINE << "CPU: starting time loop for CPU run\n";  
  clock_t begin_cpu = clock();
  for (int i=0;i<ntot;i++) {  
    sw2d_dyn_host(dt, dx, dy, g, p_eta_g, p_un_g, p_u_g, p_wet_g, p_v_g, p_vn_g, p_h_g, p_etan_g, bytes_per_word);   
    sw2d_shapiro_host(p_wet_g, p_etan_g, eps_g, p_eta_g);
    sw2d_updates_host(p_h_g , p_hzero_g, p_eta_g, p_u_g, p_un_g, p_v_g, p_vn_g, p_wet_g, hmin_g);
  }
  clock_t end_cpu = clock();
  double elapsed_secs_cpu = double(end_cpu - begin_cpu) / CLOCKS_PER_SEC;
  cout << "CPU: time loop ends\n";  
  
  //-------------------------------------------------------------------------------------------
  //SDx DEVICE RUN
  //-------------------------------------------------------------------------------------------
  cout << HLINE << "DEV: SDx device run\n";  
  cl_int err;

  // host buffers, declare and initialize
  vector<data_t,aligned_allocator<data_t>> eta  (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> etan (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> h    (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> hzero(DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> wet  (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> u    (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> un   (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> v    (DATA_SIZE);
  vector<data_t,aligned_allocator<data_t>> vn   (DATA_SIZE);
  
  data_t *p_eta   = &eta  [0]; //create pointers for use in legacy C functions
  data_t *p_etan  = &etan [0];
  data_t *p_h     = &h    [0];
  data_t *p_hzero = &hzero[0];
  data_t *p_wet   = &wet  [0];
  data_t *p_u     = &u    [0];
  data_t *p_un    = &un   [0];
  data_t *p_v     = &v    [0];
  data_t *p_vn    = &vn   [0];
  
  // INITIALIZE host data (for device run)
  sw2d_init_data_host ( p_hzero
                      , p_eta
                      , p_etan
                      , p_h
                      , p_wet
                      , p_u
                      , p_un
                      , p_v
                      , p_vn
                      , hmin
                      , bytes_per_word
                      );

  // The get_xil_devices will return vector of Xilinx Devices 
  std::vector<cl::Device> devices = xcl::get_xil_devices();
  cl::Device device = devices[0];
  
  //Creating Context and Command Queue for selected Device 
  OCL_CHECK(err, cl::Context context(device, NULL, NULL, NULL, &err));
  //The CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE allows concurrent kernels to be launched. Probablyt does no harm in single Kernel so just leaving it there without guards 
  OCL_CHECK(err, cl::CommandQueue q(context, device, CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE | CL_QUEUE_PROFILING_ENABLE, &err));
  OCL_CHECK(err, std::string device_name = device.getInfo<CL_DEVICE_NAME>(&err)); 
  std::cout << "Found Device=" << device_name.c_str() << std::endl;

  // import_binary() command will find the OpenCL binary file created using the 
  // xocc compiler load into OpenCL Binary and return as Binaries
  // OpenCL and it can contain many functions which can be executed on the
  // device.
  std::string binaryFile = xcl::find_binary_file(device_name, KERNEL_SRC_FILE_NO_EXT);
  cl::Program::Binaries bins = xcl::import_binary_file(binaryFile);
  devices.resize(1);
  OCL_CHECK(err, cl::Program program(context, devices, bins, NULL, &err));
  
  // This call will extract kernels  out of the program we loaded in the
  // previous line. 
  // Loop over array of kernels allows us to work uniformaly across single or 
  // multiple kernel scenario
  cl::Kernel **Kernels;
  Kernels = new cl::Kernel*[NKERNELS];
  for (int i=0; i<NKERNELS; i++) {
    Kernels[i] = new cl::Kernel(program, kernel_names[i], &err);
  }


  // These commands will allocate memory on the FPGA (global memory). The cl::Buffer objects can
  // be used to reference the memory locations on the device. The cl::Buffer
  // object cannot be referenced directly and must be passed to other OpenCL
  // functions.
  cout << "Creating Global Buffers..." << endl;
  OCL_CHECK(err, cl::Buffer dev_eta  (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, eta  .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_etan (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, etan .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_h    (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, h    .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_hzero(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, hzero.data(),&err));
  OCL_CHECK(err, cl::Buffer dev_wet  (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, wet  .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_u    (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, u    .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_un   (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, un   .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_v    (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, v    .data(),&err));
  OCL_CHECK(err, cl::Buffer dev_vn   (context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, size_in_bytes, vn   .data(),&err));
  
  //Two vectors, containing list of input/output device buffers
  //this list is then used to enquue tranfers to/from device memory
  std::vector<cl::Memory> inBufVec, outBufVec;
  inBufVec.push_back(dev_eta  );
  inBufVec.push_back(dev_etan );
  inBufVec.push_back(dev_h    );
  inBufVec.push_back(dev_hzero);
  inBufVec.push_back(dev_wet  );
  inBufVec.push_back(dev_u    );
  inBufVec.push_back(dev_un   );
  inBufVec.push_back(dev_v    );
  inBufVec.push_back(dev_vn   );

  //we need only eta, h, u, and v at the output (note they are input as well)
  outBufVec.push_back(dev_eta);
  outBufVec.push_back(dev_h  );
  outBufVec.push_back(dev_u  );
  outBufVec.push_back(dev_v  );

  // These commands will load the source vectors from the host
  // application and into the cl::Buffer objects. The data
  // will be be transferred from system memory over PCIe to the FPGA on-board
  // DDR memory.
  // see doc of underlying OpenCL-C command: https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clEnqueueMigrateMemObjects.html
  cout << "Copying data to Global memory" << endl;
  cl::Event write_event;  
  OCL_CHECK(err, err = q.enqueueMigrateMemObjects(inBufVec,0/* 0 means from host*/, NULL, &write_event));

  //set the kernel Arguments (since the Kernels array is a poitner array, so we have to use "->" to access its methofds
  int narg=0;
  #ifdef USE_SINGLE_KERNEL
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dt       ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dx       ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dy       ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,g        ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,eps      ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,hmin     ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_eta  ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_un   ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_u    ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_wet  ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_v    ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_vn   ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_h    ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_etan ));
    OCL_CHECK(err, err = Kernels[0]->setArg(narg++,dev_hzero));
  #endif    
 
  #ifdef USE_KERNEL_PIPES_MANUAL
    narg=0;
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_u    ));
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_v    ));
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_h    ));
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_eta  ));
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_etan ));
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_wet  ));
    OCL_CHECK(err, err = Kernels[K_MEM_RD]->setArg(narg++,dev_hzero));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_DYN1]->setArg(narg++,dt));
    OCL_CHECK(err, err = Kernels[K_DYN1]->setArg(narg++,dx));
    OCL_CHECK(err, err = Kernels[K_DYN1]->setArg(narg++,dy));
    OCL_CHECK(err, err = Kernels[K_DYN1]->setArg(narg++,g ));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_DYN2]->setArg(narg++,dt));
    OCL_CHECK(err, err = Kernels[K_DYN2]->setArg(narg++,dx));
    OCL_CHECK(err, err = Kernels[K_DYN2]->setArg(narg++,dy));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_SHAPIRO]->setArg(narg++,eps));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_UPDATES]->setArg(narg++,hmin));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_MEM_WR]->setArg(narg++,dev_u  ));
    OCL_CHECK(err, err = Kernels[K_MEM_WR]->setArg(narg++,dev_v  ));
    OCL_CHECK(err, err = Kernels[K_MEM_WR]->setArg(narg++,dev_h  ));
    OCL_CHECK(err, err = Kernels[K_MEM_WR]->setArg(narg++,dev_eta));
    OCL_CHECK(err, err = Kernels[K_MEM_WR]->setArg(narg++,dev_wet));
  #endif  

  #ifdef USE_KERNEL_PIPES_AUTO
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_0_eta_j_k_reader]->setArg(narg++,dev_eta));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_0]->setArg(narg++,dt));
    OCL_CHECK(err, err = Kernels[K_dyn_0]->setArg(narg++,dx));
    OCL_CHECK(err, err = Kernels[K_dyn_0]->setArg(narg++,dy));
    OCL_CHECK(err, err = Kernels[K_dyn_0]->setArg(narg++,g )); 
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_1_wet_j_k_reader]->setArg(narg++, dev_wet)); 
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_1_u_j_k_reader]->setArg(narg++, dev_u)); 
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_1_v_j_k_reader]->setArg(narg++, dev_v)); 
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_2_h_j_k_reader]->setArg(narg++, dev_h));     
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_2_eta_j_k_reader]->setArg(narg++, dev_eta)); 
    narg=0;
    OCL_CHECK(err, err = Kernels[K_dyn_2]->setArg(narg++,dt));
    OCL_CHECK(err, err = Kernels[K_dyn_2]->setArg(narg++,dx));
    OCL_CHECK(err, err = Kernels[K_dyn_2]->setArg(narg++,dy));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_shapiro_wet_j_k_reader]->setArg(narg++, dev_wet));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_shapiro]->setArg(narg++, eps));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_vernieuw_h_j_k_reader]->setArg(narg++, dev_h));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_vernieuw_hzero_j_k_reader]->setArg(narg++, dev_hzero));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_vernieuw]->setArg(narg++, hmin));
    narg=0;
    OCL_CHECK(err, err = Kernels[K_vernieuw_output_writer]->setArg(narg++,dev_h  ));
    OCL_CHECK(err, err = Kernels[K_vernieuw_output_writer]->setArg(narg++,dev_u  ));
    OCL_CHECK(err, err = Kernels[K_vernieuw_output_writer]->setArg(narg++,dev_v  ));
    OCL_CHECK(err, err = Kernels[K_vernieuw_output_writer]->setArg(narg++,dev_wet));
  #endif  

  //Launch the Kernel
  std::cout << "DEV: starting time loop for device run (callign kernel(s))\n";  
  clock_t begin_dev = clock();
  //time loop
  for (int i=0;i<ntot;i++) {  
    //kernel loop
    for (int j=0;j<NKERNELS;j++) {
      OCL_CHECK(err, err = q.enqueueTask(*Kernels[j], NULL, &write_event));
    }
    //wait for kernel(s) to finish for this time step
    OCL_CHECK(err, err = q.finish());
  }
  clock_t end_dev = clock();
  // The result of the previous kernel execution will need to be retrieved in
  // order to view the results. This call will write the data from the
  // buffer_result cl_mem object to the source_results vector
  cout << "Getting Results from global memory" << endl;
  OCL_CHECK(err, err = q.enqueueMigrateMemObjects(outBufVec,CL_MIGRATE_MEM_OBJECT_HOST));
  OCL_CHECK(err, err = q.finish());
  
  double elapsed_secs_dev = double(end_dev - begin_dev) / CLOCKS_PER_SEC;
  std::cout << "DEV: Device run ends\n";  

  std::cout << HLINE << "Verifying device run against host run\n";  
  verify_results ( p_h
                 , p_h_g
                 );

  std::cout << HLINE << "Peformance Comparison\n";    
  std::cout <<"CPU: execution completed in " << elapsed_secs_cpu << " seconds\n";
  std::cout <<"DEV: execution completed in " << elapsed_secs_dev << " seconds\n";

  return(1);
}



//------------------------------------------
// initialize 2D shallow-water host arrays
//------------------------------------------
// this is a legacy fnction from C, using pointers
// if you want initiaize std::vector, you must pass it
// address of first element like so: &vect[0]

void sw2d_init_data_host ( data_t *hzero
                         , data_t *eta  
                         , data_t *etan 
                         , data_t *h    
                         , data_t *wet  
                         , data_t *u    
                         , data_t *un   
                         , data_t *v    
                         , data_t *vn
                         , data_t hmin
                         , int bytes_per_word
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

    }//for
  }//for

//Initial Condition... Give eta=1 @ MID_POINT
//-------------------------------------------
*(eta+ XMID*COLS + YMID) = 1.0;

 printf("Host arrays initialized.\n");
 printf(HLINE);

}//()    

//------------------------------------------
// dyn() - the dynamics, CPU run
//------------------------------------------
void sw2d_dyn_host  ( data_t dt
                    , data_t dx
                    , data_t dy
                    , data_t g
                    , data_t *eta
                    , data_t *un
                    , data_t *u
                    , data_t *wet
                    , data_t *v
                    , data_t *vn
                    , data_t *h
                    , data_t *etan
                    , int bytes_per_word
                    ) {



//locals
//-------------------
data_t *du;// [ROWS][COLS];
data_t *dv;// [ROWS][COLS];
posix_memalign ((void**)&du, ALIGNMENT, DATA_SIZE*bytes_per_word);
posix_memalign ((void**)&dv, ALIGNMENT, DATA_SIZE*bytes_per_word);
data_t uu;
data_t vv;
data_t duu;
data_t dvv;
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


     //printf("CPU: Inside kernel_dyn, j = %d, k = %d, un = %f,vn = %f\n",j, k, un[j*COLS + k],vn[j*COLS + k]);

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
// shapiro() - filter, CPU run
//------------------------------------------
void sw2d_shapiro_host  ( data_t *wet 
                        , data_t *etan
                        , data_t eps
                        , data_t *eta
                        ) {

  //locals
  int j,k;
  data_t term1,term2,term3;

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
void sw2d_updates_host  ( data_t *h 
                        , data_t *hzero
                        , data_t *eta
                        , data_t *u
                        , data_t *un
                        , data_t *v
                        , data_t *vn
                        , data_t *wet
                        , data_t hmin
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




//------------------------------------------
// verify_results(), DEV against CPU
//------------------------------------------
int verify_results ( float* h
                   , float* h_g
){
  // verify results
  // -----------------------------------------
  printf(SLINE);

  float gold;
  float val;
  float diff;
  float eps;
  int pass = 1;

  for (int i = 0; i < ROWS; ++i)
    for (int j = 0; j < COLS; ++j) {     

      gold = *(h_g + i*COLS + j);
      val  = *(h   + i*COLS + j);
      diff = fabs(gold - val);
      eps  = EPSILON;

      if ( fabs(gold - val) > eps ) {
      printf("FAIL           @ i = %d, j = %d, expected = %f, computed = %f, diff = %f, and EPSILON = %f <== XX FAIL! xx\n"
              , i
              , j
              , gold
              , val
              , diff
              , eps
           );
        //return EXIT_FAILURE;
        pass = 0;
      }//if
      else {
#ifdef VERBOSE
        printf("Correct result @ i = %d, j = %d, expected = %f, computed = %f, diff = %f, and EPSILON = %f\n"
              , i
              , j
              , gold
              , val
              , diff
              , eps
           );
#endif
      }//else
    }//for
  if (pass)
    printf("*** TEST PASSED ***. Device results match CPU computed results!\n");
  else
    printf("*** TEST FAILED ***. Device results do not match HOST computed results!\n");
  printf(SLINE);
  return(1);
//
}
