# Generating and building the OpenCL version of the LES

The `./aux` directory contains the script `generate_and_build_OpenCL_version.pl`.
The purpose of this script is to convert sequential Fortran 77 code to Fortran 95 code with parallelised OpenCL kernels which can be offloaded to a GPU or run multithreaded on the CPU.

# Prerequisites

The script uses two compiler and an OpenCL wrapper library, all these and the other required compilers and build tools are already installed on the system.

First, a small change to the `$PATH` is required:

* In the `.bashrc`, change the `export PATH` line at the end to:

      export PATH=$OPENCL_DIR/OpenCLIntegration/bin:$RF4A_DIR/bin:$PATH

* Then source the .bashrc:

      . .bashrc

Then update the sources to the latest versions from GitHUb:

- Update the OpenCL wrapper library [OpenCLIntegration](https://github.com/wimvanderbauwhede/OpenCLIntegration)
      cd $HOME/Git/OpenCLIntegration/
      git pull
- Update and build the AutoParallel-Fortran compiler [AutoParallel-Fortran](https://github.com/wimvanderbauwhede/AutoParallel-Fortran)
      cd $HOME/Git/AutoParallel-Fortran
      git pull
      stack install
- Update the RefactorF4Acc compiler [RefactorF4Acc](https://github.com/wimvanderbauwhede/RefactorF4Acc)

      $HOME/Git/RefactorF4Acc
      git pull

Finally, update the MPI-LES code in the `debug branch

      cd MPI-LES
      git checkout debug
      git pull

Now everything is ready to build the GPU code.

# Building the LES GPU version

Once everything is up-to-date, you can build the GPU code. To build the GPU version of the LES on the DPRI system, do the following:

    perl  ./aux/generate_and_build_OpenCL_version.pl --stage autopar
    perl  ./aux/generate_and_build_OpenCL_version.pl --stage convert
    perl  ./aux/generate_and_build_OpenCL_version.pl --stage build

The final code is in the subdirectory `GeneratedCodeV2` and the executable is called `les_ocl_auto_main`. The script copies the `data` and `GIS` folders into `GeneratedCodeV2` as well.

# More info on how to use the script

Run the script in the `MPI-LES` directory as follows:

        perl ./aux/generate_and_build_OpenCL_version.pl \
            [--dev CPU|GPU] \
            [--nth #threads ] \
            [--nunits #units] \
            [--stage stage]    

The script takes the following (optional) arguments:

        --dev: the target device: CPU or GPU. Default is $plat
        --nth: the number of threads per compute unit. Default is $nth  
        --nunits: the number of compute units (--nunits). Default is $nunits
        --stage: the stage of the conversion: refactor, autopar, convert, build.

You can provide several stages separated with a comma. If not given, the script will attempt to run all stages in one go.

## Stages

1. `refactor`: Refactor the F77 code into accelerator-ready F95 code.  The refactoring source-to-source compiler `rf4a` will only run if
        - the source files have extension `.f`, `.f77`, `.F` or `.F77`, and
        - the configuration file `rf4a.cfg` is present.
2. `autopar`: Auto-parallelise the host code and generate the kernel in F95 syntax. This step requires the definition of macros used in the code, in two ways:
      - Macros to be expanded using the C preprocessor. You must define/undef these in the file `macros.h`. The script will warn if this file is not present.
      - Macros enclosing code to be skipped by the compiler. This is used  in particular because the compiler can't handle IO operations.
        You must define/undef these in the file `macros_to_skip.h`. The script will warn if this file is not present.
3. `convert`: Convert the kernel to OpenCL. The OpenCL kernel code uses two macros, the number of threads per compute unit NTH and the number of compute units NUNITS.  These can be defined using the --nth and --nunits flags or in the file `macros_kernel.h`.
4. `build`: Build the OpenCL Fortran code. The code is built using an auto-generated SConstruct file. You can of course modify this file and build the code manually. The script will not overwrite an existing `Sconstruct.auto` file.
