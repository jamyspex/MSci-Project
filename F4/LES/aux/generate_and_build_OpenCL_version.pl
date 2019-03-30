#!/usr/bin/env perl
use v5.10;
use strict;
use warnings;

# add the './aux' path to the list of include paths
BEGIN {
    push @INC, './aux';
};

use Cwd;
# A module to run the C preprocessor
use RunCpp qw( run_cpp );
# Mistery module
use RestoreStashedLines qw( restore_stashed_lines );
# This converts macro definitions from .h file to command line flag macro syntax
use MacroFileToCmdLine qw( macro_file_to_cmd_line_str );

use Data::Dumper;
use Getopt::Long;

=pod

# Name 

  generate_and_build_OpenCL_version.pl

# Description

The purpose of this script is to convert sequential Fortran 77 code to Fortran 95 code with parallelised OpenCL kernels which can be offloaded to a GPU or run multithreaded on the CPU. 

# Prerequisites

# Source code and directory structure requirements

The compilers used by the script do not work on all Fortran 77 or Fortran 95 code. However, it is usually possible with little effort to change the code and selected the correct compilation options to make it work.

## Source code requirements

# Usage
=cut

my $plat = 'GPU';
my $nth = 256;
my $nunits = 16;
my $vvv=0;

my $help_message =<<ENDH;

        $0 [-d, --dev CPU|GPU] [-s, --stage stage] [--nth #threads ] [--nunits #units]   

    The script takes the following (optional) arguments: 
        --dev: the target device: CPU or GPU. Default is $plat
        --nth: the number of threads per compute unit. Default is $nth  
        --nunits: the number of compute units (--nunits). Default is $nunits
        --stage: the stage of the conversion: refactor, autopar, convert, build.
                 
    You can provide several stages separated with a comman. 
    If not given, the script will attempt to run all stages in one go. 

    The stages are:

    1. refactor: Refactor the F77 code into accelerator-ready F95 code. 
    The refactoring source-to-source compiler `rf4a` will only run if 
        - the source files have extension `.f`, `.f77`, `.F` or `.F77`, and 
        - the configuration file `rf4a.cfg` is present.
    2. autopar: Auto-parallelise the host code and generate the kernel in F95 syntax. This step requires the definition of macros used in the code, in two ways:
        - Macros to be expanded using the C preprocessor. You must define/undef these in the file `macros.h`. The script will warn if this file is not present.
        - Macros enclosing code to be skipped by the compiler. This is used  in particular because the compiler can't handle IO operations.
        You must define/undef these in the file `macros_to_skip.h`. The script will warn if this file is not present.
    3. convert: Convert the kernel to OpenCL. 
    The OpenCL kernel code uses two macros, the number of threads per compute unit NTH and the number of compute units NUNITS. 
    These can be defined using the --nth and --nunits flags or in the file `macros_kernel.h`.
    4. build: Build the OpenCL Fortran code. The code is built using an auto-generated SConstruct file. 
    You can of course modify this file and build the code manually. The script will not overwrite an existing `Sconstruct.auto` file.
    
ENDH
my $help=0;

my $stages_str='refactor,autopar,convert,build';
my $use_separate_stash_step=0;
my $verbose;
GetOptions ('nth=i' => \$nth,                   
            'nunits=i' => \$nunits,
            'dev=s'   => \$plat,     # I know, not consistent.
            'stage=s' => \$stages_str,
            'stash' => \$use_separate_stash_step,
            'verbose'  => \$vvv,
            'help' => \$help
        ) or die("Error in command line arguments\n");

if ($help) { die $help_message; }        
my $wd = cwd();

my $VV=1;
my $vflag= $vvv ? '-b' : '';

my $main_src = 'main.f95';

# TODO: These should be extracted from the source code using rf4a. It would be best to save these to a file when running the refactoring

my @kernel_sources=qw(
adam.f95
bondv1.f95
feedbf.f95
les.f95
press.f95
velFG.f95
velnw.f95
);

# TODO: These should be extracted from the source code using rf4a

my @iowrite_subs=qw(
'anime'
);

# TODO: These should be extracted from the source code using rf4a, because they are the source used for building the code minus the kernel sources.
# However, that would probably result in many unused files being copied. So we could just copy the src folder 
#bondv1.f95
my @orig_sources=qw(
anime.f95
aveflow.f95
bondFG.f95
boundp.f95
boundsm.f95
common_sn.f95
feedbfm.f95
grid.f95
ifdata.f95
init.f95
params_common_sn.f95
set.f95
timdata.f95
timseris.f95
macros.h
macros_to_skip.h
);

#

my $iowrite_subs_str = join(' ',@iowrite_subs);
my $kernel_sources_str = join(' ',map {"./$_" } @kernel_sources);

my @sub_names = map {s/\.f95$//;$_ } @kernel_sources;

# This 30 character limit was picked ad-hoc by Gavin
my $superkernel_name = substr(join('_',@sub_names),0,30);
if (length($superkernel_name)==30) {
    $superkernel_name .= "_etc_superkernel"
} else {
    $superkernel_name .= "_superkernel"
}

my $TRUST_THE_COMPILER = 1 - $use_separate_stash_step;
    
my $skip_step_0 = 1;
my $skip_step_1 = 1;
my $skip_step_2 = 1;
my $skip_step_3 = 1;

my %stages = map { $_ => $_} split(/\s*,\s*/,$stages_str);

if (exists $stages{refactor}) {
    $skip_step_0 = 0;
}
if (exists $stages{autopar}) {
    $skip_step_1 = 0;
}
if (exists $stages{convert}) {
    $skip_step_2 = 0;
}
if (exists $stages{build}) {
    $skip_step_3 = 0;
}

my $gen_dir = 'GeneratedCode';
if ($TRUST_THE_COMPILER==1) {
    $gen_dir = 'GeneratedCodeV2';
}

# The compiler fails if this directory does not exists
if (not -d $gen_dir) {
    mkdir $gen_dir;
}


# this is LES-specific
chdir $gen_dir;
if (not -d 'data') {
    system('cp -r ../data .');
}
if (not -d 'GIS') {
    system('cp -r ../GIS .');
}
chdir $wd;
 
my $refactored=0;

# Step 0. Check if the code needs to be refactored from F77 to F95
# If so, refactor it; if not, say why not.
if (not $skip_step_0) {
    chdir 'src';
    my @f77_sources = glob('*.f77 *.F77 *.f *.F');
    my $has_F77_code = scalar @f77_sources > 0;
    my $has_rf4a_cfg = -e './rf4a.cfg';
    if ( $has_rf4a_cfg and $has_F77_code) {
        $refactored=1;

        say "Refactoring F77 code into accelerator-ready F95 code";
        say($ENV{HOME}.'/Git/RefactorF4Acc/bin/'.'refactorF4acc.pl -c ./rf4a.cfg '.$vflag); 
        system($ENV{HOME}.'/Git/RefactorF4Acc/bin/'.'refactorF4acc.pl -c ./rf4a.cfg '.$vflag); 
 
    } else {
        say "Refactoring step stage skipped because already done:\n";
        if (!$has_rf4a_cfg) {
        say "\t- No rf4a.cfg file"; 
        }
        if(!$has_F77_code) {
            say "\t-No F77 source files";
        }
        say '';
    }
}
    my $src_dir = $refactored ? 'RefactoredSources' : 'src';

# Step 1. Run the auto-parallelizing GPU compiler `AutoParallel-Fortran-exe`. The output is stored in `GeneratedCodeV2`
if (not $skip_step_1) {
    if ($TRUST_THE_COMPILER) {
        chdir $src_dir;
        ##
        say'*NOTE 2018-03-07* 
        The `AutoParallel-Fortran` compiler has built-in handling of macros via the -D and -X flags. 
        This generates the same code as when using the `run_cpp.pl` and `restore_stashed_lines.pl` scripts. 
        ' if 0;
        
        (my $defined_macros_str, my $undef_macros_str) = macro_file_to_cmd_line_str( './macros.h','-D');
        (my $macros_to_skip_str, my $empty_str) = macro_file_to_cmd_line_str('./macros_to_skip.h','-X');
        
        say("AutoParallel-Fortran-exe $kernel_sources_str -out ../$gen_dir/ -iowrite $iowrite_subs_str -main ./$main_src -plat $plat  $defined_macros_str $macros_to_skip_str $vflag" );
        #system('which AutoParallel-Fortran-exe');die;
        system("AutoParallel-Fortran-exe $kernel_sources_str -out ../$gen_dir/ -iowrite $iowrite_subs_str -main ./$main_src  -plat $plat  $defined_macros_str $macros_to_skip_str $vflag" );    
        
    } else {    
    
        say '* First, in `'.$src_dir.'`, run CPP on the code using the macros in `macros.h` and stash lines guarded with macros from `macros_to_skip.h`. This generates the file `stash.pl`' if $VV;
        
        chdir $src_dir;
        
        run_cpp();
          
        ##
        say '* Then, in `PostCPP`, run the OpenCL compiler `AutoParallel-Fortran-exe`. This will take a while and produce a lot of output, which you can ignore.' if $VV;
        
        chdir $wd;
        if (not -d 'PostCPP') {
            mkdir 'PostCPP';
        }
         
        chdir 'PostCPP';
        say("AutoParallel-Fortran-exe $kernel_sources_str -out ../$gen_dir/ -iowrite $iowrite_subs_str -main ./$main_src $vflag -plat $plat");
        system("AutoParallel-Fortran-exe $kernel_sources_str -out ../$gen_dir/ -iowrite $iowrite_subs_str -main ./$main_src $vflag -plat $plat");
        
        ##
        say "* In '$gen_dir', we restore code segments that were stashed in the previous step" if $VV;
        chdir $wd;
        chdir $gen_dir;
        
        restore_stashed_lines("$wd/$src_dir/stash.pl"); 
        system('cp ./PostGen/* .');
    
    }
}
# Step 2. Copy non-modified source files and scripts and config files needed to build the OpenCL kernel, and generate the OpenCL kernel
if (not $skip_step_2) { 
    
    ##
    say "* In `$gen_dir`, we copy the non-modified source files into the current folder, as well as some scripts and config files needed to build the OpenCL kernel." if $VV;
    chdir $wd;
    chdir $gen_dir;
    
    
    
    my $ref_dir = $TRUST_THE_COMPILER ? "$wd/$src_dir" : "$wd/PostCPP";
    for my $src (@orig_sources) {
       system("cp $ref_dir/$src ."); 
    }

    ## TODO rf4a_to_C.cfg should be generated
    my $rf4a_to_C_cfg = <<ENDCFG;
MODULE = module_${superkernel_name}
MODULE_SRC = module_${superkernel_name}.f95
TOP = ${superkernel_name}
KERNEL = ${superkernel_name}
PREFIX = .
SRCDIRS = .
NEWSRCPATH = ./Temp
EXCL_SRCS = (module_${superkernel_name}_init|_host|\\.[^f])
EXCL_DIRS = ./PostCPP,./Temp
MACRO_SRC = macros_kernel.h

ENDCFG
    
    open my $CFG, '>', 'rf4a_to_C.cfg';
    print $CFG $rf4a_to_C_cfg;
    close $CFG;
    
    my @sources2=qw(
    macros_kernel.h
    array_index_f2c1d.h
    );
    
    my $ref_dir_2 = "$wd/aux";
    for my $src (@sources2) {
        system("cp $ref_dir_2/$src . ");
    }

    ##
    say '* Then we generate the actual OpenCL kernel code using `RefactorF4Acc`' if $VV;
    chdir $wd;
    chdir $gen_dir;
    
    my $macros_kernel_src = './macros_kernel.h';
    
    if (not -e $macros_kernel_src ) {
        say "No `macros_kernel.h` file for the macros NTH and NUNITS";
        say "Creating one with NTH=$nth and NUNITS=$nunits";
        open my $MKS, '>', 'macros_kernel.h';
        say $MKS "#define NTH $nth";
        say $MKS "#define NUNITS$nunits";
        close $MKS;

    }
    say($ENV{HOME}.'/Git/RefactorF4Acc/bin/'.'refactorF4acc.pl '.$vflag.' -P translate_to_OpenCL -c rf4a_to_C.cfg '.$superkernel_name); 
    system($ENV{HOME}.'/Git/RefactorF4Acc/bin/'.'refactorF4acc.pl '.$vflag.' -P translate_to_OpenCL -c rf4a_to_C.cfg '.$superkernel_name);
    system("cp  module_$superkernel_name.cl module_${superkernel_name}_ORIG.cl");

    # Unused, for debugging
    #open my $MK, '<', $macros_kernel_src or die $!;
    #my @ls=<$MK>;
    #close $MK;
    #my $macros_str=join(" ",map {
    #    $_=~s/\n//;
    #    s/^\s*//;
    #    s/\s*$//;
    #    s/.define\s*/-D/;
    #    s/.undef\s*/-U/;
    #    s/\s+/=/;
    #    $_
    #} @ls);
    #say("cpp $macros_str -I. -P module_$superkernel_name.cl > module_${superkernel_name}_after_CPP_for_debugging.cl");
    #system("cpp $macros_str -I. -P module_$superkernel_name.cl > module_${superkernel_name}_after_CPP_for_debugging.cl");

}

# Step 3. Build the host code for the OpenCL kernel
if (not $skip_step_3) {
##
    chdir $wd;
    chdir $gen_dir;
    
    ## SConstruct.auto is generated
    create_sconstruct($main_src, \@kernel_sources, \@orig_sources, $superkernel_name);
    
    say  "Now we can build the OpenCL Fortran host code, setting the number of threads and compute units depending on the GPU";
    say 'Note that the Scons build runs cpp on the kernel for the macros NTH, NUNITS and BARRIER_OK';
    say 'Normally these are set via the `nth`, `nunits` and `dev` flags on the scons command line';
    say 'But you can also put them in `macros_kernel.h`';
    
    say("scons -f SConstruct.auto -s mcm=m dev=$plat nth=$nth nunits=$nunits");
    system("scons -f SConstruct.auto -s mcm=m dev=$plat nth=$nth nunits=$nunits");
}

# ----- Helper functions -----
# 
sub create_sconstruct { (my $main_src, my $kernel_sources, my $orig_sources, my $superkernel_name)=@_;

    my @host_srcs = map {
        my $name = strip_ext($_);
        my $host_name = $name.'_host';
        my $host_src = "'$host_name.f95'";
        $host_src
    } ($main_src, @{ $kernel_sources } );

    my $host_srcs_str = join(',',@host_srcs);
    my @q_orig_sources = map { "'./$_'" } @{ $orig_sources };
    my $orig_srcs_str = join(',',@q_orig_sources);

    my $module_init_str = "'./module_${superkernel_name}_init.f95'";
    my $kernel_src_cl_str = "'module_${superkernel_name}.cl'";

    if (not -e "SConstruct.auto" ) {
        open my $SCONS_TEMPL,'<',"$wd/aux/SConstruct.templ";    
        open my $SCONS,'>', "SConstruct.auto";
        while (my $line= <$SCONS_TEMPL> ) {
            $line=~/__HOST_SRCS__/ && do {
                $line=~s/__HOST_SRCS__/$host_srcs_str/;            
            }; 
            $line=~/__MODULE_INIT__/ && do {
                $line=~s/__MODULE_INIT__/$module_init_str/;
            }; 
            $line=~/__KERNEL_SRC_CL__/ && do {
                $line=~s/__KERNEL_SRC_CL__/$kernel_src_cl_str/;
            };
            $line=~/__ORIG_SOURCES__/ && do {
                $line=~s/__ORIG_SOURCES__/$orig_srcs_str/;
            }; 
            print $SCONS $line;
        }
        close $SCONS;
        close $SCONS_TEMPL;
    } else {
        say "SConstruct.auto already exists, not overwriting. Delete or rename the file and run the build stage again.";
    }

}

sub strip_ext { (my $fn)=@_;
    $fn=~s/\.\w+$//;
    return $fn;
}
