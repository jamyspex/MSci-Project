# AutoParallel-Fortran

A domain specific, automatically parallelising source-to-source compiler for Fortran-95 that takes scientific Fortran as input and produces Fortran code parallelised using the OpenCL framework.
The Fortran parser used for this compiler is _Language-Fortran_, a Haskell based Fortran parser. The original parser is available at [https://github.com/dagit/language-fortran](https://github.com/dagit/language-fortran); the current project contains a modified version.

This is research code so it is not production ready.

## Installation

This project requires the [Glasgow Haskell Compiler](https://www.haskell.org/) (GHC) version 7.8 or later and the Haskell tool  [Stack](https://docs.haskellstack.org/en/stable/README/). To build the code:

    stack build
    stack install

If you change the `language-fortran` parser, do the following:

    cd language-fortran
    runhaskell Setup.hs configure
    runhaskell Setup.hs build  

Then copy the file `Lexer.hs` from  `dist/build/Language/Fortran` to `src/Language/Fortran`.  

    cp dist/build/Language/Fortran/Lexer.hs src/Language/Fortran

The `stack install` command will install the binary `AutoParallel-Fortran-exe` in `$HOME/.local/bin` so make sure that this folder is in your `$PATH`.    

## Using the compiler

The compiler is a command line tool. There are a number of command line arguments, but most are optional. For example, to run the compiler on a codebase whose main program is in 'main.f95' with subroutines located in 'subroutines.f95':

    AutoParallel-Fortran-exe ./shapiro.f95 ./dyn.f95 ./update.f95 -out ../Autopar/ -main ./main.f95 -v -plat $1 -X NO_IO -v

The following flags are defined:

      - `-modules`: Optional flag followed by the space-separated list of module source files.
      - `-out`: The output directory for the generated code. The name of the kernel file is derived from the original filenames. Omitting this argument results in a the output being saved in the current directory.
      - `-main`: The name of the source file containing the main program.
      - `-plat`: The OpenCL platform. Currently either `GPU` or `CPU`.      
      - `-D`: Defined macros. Takes a space-separared list of C preprocessor macros, e.g. -D VERBOSE DBG. These macros will be defined when CPP is run.
      - `-X`: Excluded macros. Takes a space-separared list of C preprocessor macros, e.g. -X NO_IO NO_OUTPUT. These macros will be excluded when CPP is run, i.e. they will still be present in the final code.
      - `-v`: Verbose output, obstacles to parallelisation are reported to the user.      
      - `-ffixed-form`: for Fortran code in fixed-form format. It enforces that input lines must be no more than 72 characters long. Output is also formatted as fixed for (6 leading spaces on each line and no more than 72 characters per line).
      - `-lfb` defines a value for the loop fusion bound. That is, the difference in iterator end value that is allowed for two loops to be fused. Not including this      

 All module subroutines will be combined into a single "superkernel-style" kernel.

## OpenCL/C code generation

The compiler generates Fortran code in two parts: host code using the [_OclWrapper_ Fortran OpenCL API](https://github.com/wimvanderbauwhede/OpenCLIntegration) and kernel code in Fortran. OpenCL does not support Fortran so this code needs to be translated to OpenCL C code. This is done using [a separate compiler](https://github.com/wimvanderbauwhede/RefactorF4Acc) as follows:

### Create a config file

The AutoParallel-Fortran compiler will create a module containing the kernel files, with the name based on the original source code filename. Create a file `rf4a.cfg` in the folder containing the generated module:

    MODULE = module_<orig src name>_superkernel
    MODULE_SRC = module_<orig src name>_superkernel.f95
    TOP = <orig subroutine name>_superkernel
    KERNEL = <orig subroutine name>_superkernel
    PREFIX = .
    SRCDIRS = .  
    # A regex specifying which source files should be skipped
    EXCL_SRCS = (module_sub_superkernel_init|main_host|sub_host|\.[^f])
    EXCL_DIRS = < any folder that should not be search for source files >

### Now run the OpenCL translation

    <path to script>/refactorF4acc.pl -P translate_to_OpenCL -c rf4a.cfg

## Modification of Parser and Lexer

The parser used by this project was generated from a grammar file using the _Happy_ parser generator and _Alex_ lexer. Therefore, to generate the Haskell sources after making changes to the parser or lexer (`Parser.y` or `Lexer.x`) requires the installation of [happy](https://www.haskell.org/happy/#download) and [alex](https://www.haskell.org/alex/).


### Colofon

This code was originally developed by Gavin Davidson for his Masters Computing Science dissertation project at the University of Glasgow, with me as supervisor, and latersextensively modified by me.

For more details about the project, please read [Gavin's dissertation](https://github.com/wimvanderbauwhede/AutoParallel-Fortran/blob/master/docs/dissertation_Gavin_Davidson_2016.pdf).
