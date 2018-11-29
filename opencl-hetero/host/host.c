// ======================================================
// Hetero-OpenCL for 2D shallow water model 
// Primary target is FPGA, but also meant to work with 
// GPUs and CPUs
// By: Syed Waqar Nabi, Glasgow
// 2016.11.29
//
// ======================================================

//application specific header
#include "host-sw2d.h"
  //this application-specific header file (should) includes the generic host header


// -------------------------------------
// Globals
// -------------------------------------
  //timing variables
  double time_write_to_device[NTIMES];
  double time_total_alltimesteps_kernels[NTIMES];
  double time_read_from_device[NTIMES];
  double time_write2file;
  double time_total_alltimesteps_kernels_onhost;

////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(int argc, char** argv) {
  using namespace std;

  
  // =============================================================================
  // Generic Opencl/Local variables
  // =============================================================================
  int       quantum, checktick();
  double    t;
  int       k;
  int       BytesPerWord = sizeof(stypeHost);
  ssize_t   i,j;

  //for timing profile
  double start_timer, end_timer;

  printf(SLINE); 
  printf(SLINE);  
  printf("2D shallow water heterogenuous opencl model $\n"); printf(HLINE);
  printf(SLINE); 

// =============================================================================
// CONSTANTS
// =============================================================================
   int ntot = NTOT; //how many time steps
   int nout = 5;    //log output after every how many steps?
   int rows = ROWS;
   int cols = COLS;
  
  //scalars
   stypeHost hmin = 0.05;
   stypeHost dx = 10.0;
   stypeHost dy = 10.0;
   stypeHost dt = 0.1;
   stypeHost g = 9.81;  
   stypeHost eps = 0.05;
   stypeHost hmin_g = 0.05; //golden copies not really needed, but to avoid bugs
   stypeHost dx_g = 10.0;
   stypeHost dy_g = 10.0;
   stypeHost dt_g = 0.1;
   stypeHost g_g = 9.81;  
   stypeHost eps_g = 0.05;

// =============================================================================
// Do a HOST-only, GOLDEN run (this is always done, irrespective of target)
// =============================================================================
  printf(SLINE); 
  printf ("*** HOST Run ***\n");
  printf(SLINE); 

  //arrays
  stypeHost *hzero_g, *eta_g  ,*etan_g ,*h_g    ,*wet_g  ,*u_g    ,*un_g   ,*v_g    ,*vn_g;   

  posix_memalign ((void**)&eta_g  , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&etan_g , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&h_g    , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&hzero_g, ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&wet_g  , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&u_g    , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&un_g   , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&v_g    , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&vn_g   , ALIGNMENT, SIZE*BytesPerWord);

  //initialize arrays
  //-------------------------
  sw2d_init_data_host(hzero_g, eta_g, etan_g, h_g, wet_g, u_g, un_g, v_g, vn_g, hmin, BytesPerWord);

#ifdef LOGRESULTS
  //write initial distribution of eta 
  FILE * feta0_g;
  feta0_g= fopen ("eta0_g.dat","w");
  for (int i = 0; i < ROWS; ++i) {
    for (int j = 0; j < COLS; ++j) {    
      fprintf(feta0_g,"%f,  ", *(eta_g+ i*COLS + j));
    }//j
    fprintf(feta0_g,"\n");
  }//i
  fclose(feta0_g);  
#endif

  // determine maximum water depth
  stypeHost hmax_g= 0.0;
  for (int j=1; j<= COLS-2; j++) {
    for (int k=1; k<=ROWS-2; k++) {
      hmax_g = MAX (hmax_g, *(h_g + j+COLS + k));
    }
  }
  //maximum phase speed
  stypeHost c_g = sqrt(2*g*hmax_g);
  
  //determine stability parameter
  stypeHost lambda_g = dt*sqrt(g*hmax_g)/MIN(dx,dy);
  
  printf ("Host: starting time loop for host run\n");
  start_timer = mysecond();
  for (int i=0;i<ntot;i++) {  
    //call dyn (host version)
    //-------------------------
    sw2d_dyn_host(dt, dx, dy, g, eta_g, un_g, u_g, wet_g, v_g, vn_g, h_g, etan_g, BytesPerWord); 
  
    //call shapiro (host version)
    //---------------------------
    sw2d_shapiro_host(wet_g, etan_g, eps_g, eta_g);

    //call updates (host version. in the original this is done in main)
    //-------------------------------------------------------------------
    sw2d_updates_host  (h_g , hzero_g, eta_g, u_g, un_g, v_g, vn_g, wet_g, hmin_g);
  }
  
  end_timer = mysecond();
  time_total_alltimesteps_kernels_onhost = end_timer - start_timer;
  printf ("Host: host execution complete.\n");

  
#ifdef OCL
  printf(SLINE); 
  printf ("OCL: *** OpenCL Run ***\n");
  printf(SLINE); 

// =============================================================================
// DEVICE RUN: 
// (includes APPLICATION execution loop; if we want multiple observations)
// =============================================================================


  //opencl variables
  cl_int            err = CL_SUCCESS;
  cl_context        context;            // compute context
  //cl_program        program;            // compute program
  cl_program        programs[NPROGRAMS];            // compute program(s)
  cl_command_queue  commands[NKERNELS]; // compute command queue
  cl_kernel         kernels[NKERNELS];  // compute kernels
//  cl_command_queue  commands;           // compute command queue
//  cl_kernel         kernel;         // compute kernels

  // =============================================================================
  // Application-specific Opencl variables
  // =============================================================================
  //device buffers
  cl_mem 
     dev_hzero
    ,dev_eta  
    ,dev_etan 
    ,dev_h    
    ,dev_wet  
    ,dev_u    
    ,dev_un   
    ,dev_v    
    ,dev_vn;

  //arrays
  stypeHost *hzero,*eta  ,*etan ,*h    ,*wet  ,*u    ,*un   ,*v    ,*vn;   

  posix_memalign ((void**)&hzero, ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&eta  , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&etan , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&h    , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&wet  , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&u    , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&un   , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&v    , ALIGNMENT, SIZE*BytesPerWord);
  posix_memalign ((void**)&vn   , ALIGNMENT, SIZE*BytesPerWord);


  //initialize host arrays
  //-------------------------
  //needs to happen again as host run would have made some arrays dirty
  sw2d_init_data_host(hzero, eta, etan, h, wet, u, un, v, vn, hmin, BytesPerWord);
  //write initial distribution of eta 
#ifdef LOGRESULTS
  FILE * feta0;
  feta0= fopen ("eta0.dat","w");
  for (int i = 0; i < ROWS; ++i) {
    for (int j = 0; j < COLS; ++j) {    
      fprintf(feta0,"%f,  ", *(eta+ i*COLS + j));
    }//j
    fprintf(feta0,"\n");
  }//i
  fclose(feta0);  
#endif  
  
  // determine maximum water depth
  stypeHost hmax= 0.0;
  for (int j=1; j<= COLS-2; j++) {
    for (int k=1; k<=ROWS-2; k++) {
      hmax = MAX (hmax, *(h + j+COLS + k));
    }
  }
  
  //maximum phase speed
  stypeHost c = sqrt(2*g*hmax);
  
  //determine stability parameter
  stypeHost lambda = dt*sqrt(g*hmax)/MIN(dx,dy);
  printf("c = %f, lambda = %f\n", c, lambda);


// =============================================================================
// Setup for OCL if applicable
// =============================================================================
  //checks clock precision etc 
  oclh_timing_setup(u,  BytesPerWord);

  //display setup
  oclh_display_setup();

  //initialize opencl; create context, commansds, program, and kernel
  //oclh_opencl_boilerplate(&context, &commands, &program, &kernel, argc, argv);
  //oclh_opencl_boilerplate(&context, &commands, &program, kernels, argc, argv);
  //oclh_opencl_boilerplate(&context, commands, &program, kernels, argc, argv);
  oclh_opencl_boilerplate(&context, commands, programs, kernels, argc, argv);
  
  //create read-write buffers of size SIZE on device
  oclh_create_cldevice_buffer(&dev_hzero, &context, CL_MEM_READ_WRITE);
  oclh_create_cldevice_buffer(&dev_eta  , &context, CL_MEM_READ_WRITE);
  oclh_create_cldevice_buffer(&dev_h    , &context, CL_MEM_READ_WRITE);
  oclh_create_cldevice_buffer(&dev_wet  , &context, CL_MEM_READ_WRITE);
  oclh_create_cldevice_buffer(&dev_u    , &context, CL_MEM_READ_WRITE);
  oclh_create_cldevice_buffer(&dev_v    , &context, CL_MEM_READ_WRITE);
  oclh_create_cldevice_buffer(&dev_etan , &context, CL_MEM_READ_WRITE); //not needed when using channels?
  oclh_create_cldevice_buffer(&dev_un   , &context, CL_MEM_READ_WRITE); //not needed when using channels?
  oclh_create_cldevice_buffer(&dev_vn   , &context, CL_MEM_READ_WRITE); //not needed when using channels?

    //-----------------------------------------------
    // Write arrays to device memory, if applicable
    //-----------------------------------------------
    //NOTE: the host-device transfer is OUTSIDE time loop of the application
    //      but INSIDE the NTIMES loop as we want to run multiple, independent 
    //      experiments.

//==============================================================================
for (int run=0; run<NTIMES; run++) {
//==============================================================================
printf(SLINE); printf("Application Run # %d\n",run+1);

    sw2d_init_data_host(hzero, eta, etan, h, wet, u, un, v, vn, hmin, BytesPerWord);


    // Record times 
    start_timer = mysecond();
    oclh_blocking_write_cl_buffer(commands, &dev_hzero, hzero);
    oclh_blocking_write_cl_buffer(commands, &dev_eta  , eta  );
    oclh_blocking_write_cl_buffer(commands, &dev_etan , etan );
    oclh_blocking_write_cl_buffer(commands, &dev_h    , h    );
    oclh_blocking_write_cl_buffer(commands, &dev_wet  , wet  );
    oclh_blocking_write_cl_buffer(commands, &dev_u    , u    );
    oclh_blocking_write_cl_buffer(commands, &dev_un   , un   );
    oclh_blocking_write_cl_buffer(commands, &dev_v    , v    );
    oclh_blocking_write_cl_buffer(commands, &dev_vn   , vn   );

    //end_timer = mysecond();
    //time_write_to_device[run] = end_timer - start_timer;

    //printf("OCLH: Device buffers written\n");

  //-----------------------------------------------
  // Set args, global/local sizes (OCL only)
  //-----------------------------------------------
  //set the arguments 
  oclh_set_kernel_args  ( kernels
                        , &dt
                        , &dx
                        , &dy
                        , &g
                        , &eps
                        , &hmin
                        , &dev_eta
                        , &dev_un
                        , &dev_u
                        , &dev_wet
                        , &dev_v
                        , &dev_vn
                        , &dev_h
                        , &dev_etan
                        , &dev_hzero
//                        , &rows
//                        , &cols                        
                    );
  //printf("OCLH: Arguments set\n");
  end_timer = mysecond();
  time_write_to_device[run] = end_timer - start_timer;

  //set global and local sizes
  size_t globalSize[] = {0,0,0};
  size_t localSize[]  = {0,0,0};
  oclh_get_global_local_sizes(globalSize, localSize);

// ========================================================================
// TIME LOOP (Kernel Execution)
// ========================================================================
      printf("Starting time loop \n"); 
      start_timer = mysecond();
      // top level (time) loop is on the host
      for (int i=0;i<ntot;i++) {
        oclh_enq_cl_kernel(commands, kernels, globalSize, localSize);
      }
      //One complete execution of application (all time steps)  ends here
// ========================================================================

      end_timer = mysecond();
      time_total_alltimesteps_kernels[run] = end_timer - start_timer;
      //times[k] = mysecond() - times[k];

    //-------------------------------------------
    // Read back the results
    //-------------------------------------------
    start_timer = mysecond();

    printf("Reading from device buffers\n");
    oclh_blocking_read_cl_buffer(commands, &dev_eta, eta);
    oclh_blocking_read_cl_buffer(commands, &dev_h  , h);
    oclh_blocking_read_cl_buffer(commands, &dev_u  , u);
    oclh_blocking_read_cl_buffer(commands, &dev_v  , v);
    end_timer = mysecond();
    time_read_from_device[run] = end_timer - start_timer;
    printf("End of Run\n"); printf(HLINE);
}//for (run=0; k<NTIMES; k++)

  // =============================================================================
  // POST PROCESSING
  // ============================================================================= 
  //log only once after all NTIMES loops. 
  //oclh_log_results(eta_g, h_g, u_g, v_g, hzero);
#ifdef LOGRESULTS
  oclh_log_results(eta, h, h_g, u, v, hzero);
#endif
  //clReleaseObject(dev_hzero);
  //clReleaseObject(dev_eta  );
  //clReleaseObject(dev_etan );
  //clReleaseObject(dev_h    );
  //clReleaseObject(dev_wet  );
  //clReleaseObject(dev_u    );
  //clReleaseObject(dev_un   );
  //clReleaseObject(dev_v    );
  //clReleaseObject(dev_vn   );
  // Calculate BW. Display and write to file
  oclh_calculate_performance();
  oclh_verify_results(h, h_g);

  for (int  i=0; i<NPROGRAMS; i++)
    clReleaseProgram(programs[i]);
  //clReleaseKernel(kernel); 
  //clReleaseCommandQueue(commands);

  clReleaseContext(context);

  free(hzero);
  free(eta  );
  free(etan );
  free(h    );
  free(wet  );
  free(u    );
  free(un   );
  free(v    );
  free(vn    );
#endif
  //#ifdef OCL

//   
//   // Write output arrays 
//#ifdef LOGRESULTS
//#endif

//   //verify results
//   start_timer = mysecond();

//   end_timer = mysecond();
//   time_verify = end_timer - start_timer;
// 

 
   // Display overall timing profile 
   //oclh_disp_timing_profile();

// Shutdown and cleanup
// -----------------------------------------
  free(hzero_g);
  free(eta_g  );
  free(etan_g );
  free(h_g    );
  free(wet_g  );
  free(u_g    );
  free(un_g   );
  free(v_g    );
  free(vn_g    );

  printf(SLINE); 
  printf("Executable ends\n");
  printf(SLINE); 


}

