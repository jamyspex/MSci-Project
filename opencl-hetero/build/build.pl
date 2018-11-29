#!/usr/bin/perl -w

use strict;
use warnings;
use Getopt::Long;   #for command line options
use File::Slurp;
use File::Copy qw(copy);
use File::Path qw(make_path remove_tree);
#use Time::HiRes qw( usleep ualarm gettimeofday tv_interval nanosleep
#          clock_gettime clock_getres clock_nanosleep clock
#                      stat);
use IO::Tee;
use IO::File;

#my $start = Time::HiRes::gettimeofday();

#----------------------------------------------------------
# default options, and list of possible command line options
#----------------------------------------------------------
our $kernelSourceFile='Kernel.source.cl';
our $target = 'CPU'; #[AOCL/AOCL_CHREC/SDACCEL/GPU/CPU/MAXELER]
our $numNovoGdevices = 1; #Only relevant when target==AOCL_CHREC
our $ROWS = 53;
our $COLS = $ROWS;
our $make = 'all';     #[run/host/aoclbin/clean]
our $FLOW = 'cpu_emu'; #[cpu_emu/hw/hw_emu] 
our $vectorSize = 1;   #scalar by default
our $help = '';
our $wordType = 'INT'; #[INT/FLOAT/DOUBLE] 
our $batch = '';
our $logresults = '';
our $noUniqueTargetDir = '';
our $workitems = 'SINGLEWI';

our $NTOT   = 1000; #time steps
our $NTIMES = 10;   #multiple simulation runs

#estimated by default
our $estimate = '';

#loop unroll
our $unrollFull= '';
our $unrollFactor = '';

#required work-group size
#our $reqWorkgroupSize = '1,0,0';
our $reqWorkgroupSize = '';

#AOCL specific?
our $numSimdItems = 1;
our $numComputeUnits = 1;
our $noInterleaving = '';
our $useChannels = '';

#SDACCEL specific
our $xclpipelineWorkitems='';
our $xclpipelineLoop='';
our $xclmaxmemoryports='';
our $xclmemportwidth='';

#-----------------------------
#get options from command line
#-----------------------------
GetOptions (
    'tar=s' =>  \$target
  , 'row=s' =>  \$ROWS               
  , 'col=s' =>  \$COLS
  , 'wi=s'  =>  \$workitems
  , 'w=s'   =>  \$wordType
  , 'ntot=s'=>  \$NTOT
  , 'ntimes=s'   =>  \$NTIMES
  , 'v=s'   =>  \$vectorSize
  , 'r=s'   =>  \$reqWorkgroupSize    
  , 'ch'    =>  \$useChannels
  , 'm=s'   =>  \$make
  , 'h'     =>  \$help
  , 'log'   =>  \$logresults
  , 'batch' =>  \$batch
  , 'nut'   =>  \$noUniqueTargetDir
  , 'nd=s'  =>  \$numNovoGdevices
  , 'f=s'   =>  \$FLOW                  #AOCL and SDACCEL only          
  , 'uf'    =>  \$unrollFull            #AOCL and SDACCEL only          
  , 'un=s'  =>  \$unrollFactor          #AOCL and SDACCEL only          
  , 's=s'   =>  \$numSimdItems          #AOCL only
  , 'c=s'   =>  \$numComputeUnits       #AOCL only
  , 'ni'    =>  \$noInterleaving        #AOCL only
  , 'e'     =>  \$estimate              #SDACCEL only
  , 'xpw'   =>  \$xclpipelineWorkitems  #SDACCEL only
  , 'xpl'   =>  \$xclpipelineLoop       #SDACCEL only
  , 'xmm'   =>  \$xclmaxmemoryports     #SDACCEL only
  , 'xmw=s' =>  \$xclmemportwidth       #SDACCEL only
  );

my $arraySize = $COLS*$ROWS;

if($help) {
  printHelp();
  exit;
}

#Kernel source file depending on options
if ($target eq 'AOCL_CHREC') {$kernelSourceFile='Kernel.singlewi.wchannels.novogcluster.source.cl';} 
else {
  if    ($workitems eq 'SINGLEWI') {
    if($useChannels)  {$kernelSourceFile='Kernel.singlewi.wchannels.source.cl';} 
    else              {$kernelSourceFile='Kernel.singlewi.source.cl';} 
  }                                    
  elsif ($workitems eq 'NDRANGE')  { $kernelSourceFile='Kernel.ndrange.source.cl';}
  else                             {die "Invalid workitems (-wi) specification."};
}

#convenience booleans
my $FPGA   = ( ($target eq 'SDACCEL') || ($target eq 'AOCL') || ($target eq 'MAXELER')    || ($target eq 'AOCL_CHREC'));
my $ocl    = ( ($target eq 'SDACCEL') || ($target eq 'AOCL') || ($target eq 'AOCL_CHREC') || ($target eq 'CPU') || ($target eq 'GPU'));
my $hostonly= ($target eq 'HOST');

#relevant for sdaccel; if flow is hardware, turn on estimation no matter what
#flag is passed
$estimate = 1 if ($FLOW eq 'hw');

#--------------------------------------------------
#Now move to build directory of appropriate target
#--------------------------------------------------
my $targetPlatformDirectory;
if    ($target eq 'AOCL')       {$targetPlatformDirectory = '../build-aocl';}
elsif ($target eq 'AOCL_CHREC') {$targetPlatformDirectory = '../build-aocl-chrec';}
elsif ($target eq 'SDACCEL')    {$targetPlatformDirectory = '../build-sdaccel';}
elsif ($target eq 'GPU')        {$targetPlatformDirectory = '../build-nvidia-gpu';}
elsif ($target eq 'CPU')        {$targetPlatformDirectory = '../build-intel-cpu';}
elsif ($target eq 'HOST')       {$targetPlatformDirectory = '../build-host-only';}
else                            {die "Invalid target specificaion"};
print "Moving to build folder $targetPlatformDirectory for target: $target\n";
chdir $targetPlatformDirectory;

#--------------------------------------------------
#Create custom dirctory name for this build, 
#--------------------------------------------------
#get local time to create unique folder name
my $sec, my $min, my $hour, my $mday, my $mon, my $year, my $wday, my $yday, my $isdst;
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
                                                localtime(time);

my $timeStamp =$yday.".".$hour.$min.$sec;

#create unique target dir or over-write generic one, depending on option
my $targetDir;
if ($noUniqueTargetDir) {
  $targetDir = "build-current"
}
else {
  $targetDir 
    = "build-".$target
    ."-".$timeStamp
    ."-".$wordType.$vectorSize
    ."-".$ROWS
    ."-".$FLOW
    ."-".$workitems
    ;
  $targetDir=$targetDir."-CHANNELS" if($useChannels);
};

#--------------------------------------------
# create target directory
#--------------------------------------------
print "\nBUILD.PL: Making target directory $targetDir...\n";
make_path($targetDir);


#----------------------------------------------------
#print pre-build messages and store them in LOG file
#----------------------------------------------------
my $buildparamlogfile = $targetPlatformDirectory."/".$targetDir."/build-parameters.log";
open(my $bplfh, '>', $buildparamlogfile)
  or die "BUILD.PL: Could not open file '$buildparamlogfile' $!";  
my $tee = new IO::Tee(  \*STDOUT , $bplfh);

print $tee "Building with the following options...\n";
print $tee "--------------------------------------\n";
print $tee "TARGET                 = $target\n";
print $tee "WORD                   = $wordType\n";
print $tee "ROWS                   = $ROWS\n";
print $tee "COLS                   = $COLS\n";
print $tee "NTOT                   = $NTOT\n";
print $tee "NTIMES                 = $NTIMES\n";
print $tee "SIZE                   = $arraySize\n";
print $tee "VECTOR_SIZE            = $vectorSize\n";
print $tee "WORKITEMS              = $workitems\n";
print $tee "REQ_WORKGROUP_SIZE     = $reqWorkgroupSize\n" if($reqWorkgroupSize);
print $tee "Make                   = $make \n";
print $tee "targerDir              = $targetDir\n";
print $tee "timeStamp              = $timeStamp\n";
print $tee "LOGRESULTS             = $logresults\n" if($logresults);
print $tee "USECHANNELS            = $useChannels\n" if($useChannels);
print $tee "Source Kernel File     = $kernelSourceFile\n";
print $tee "NDEVICES               = $numNovoGdevices\n" if($target eq 'AOCL_CHREC');

#AOCL and SDACCEL only
print $tee "FLOW                   = [AOCLS and SDACCEL ONLY] $FLOW \n";
print $tee "UNROLL_FULL            = [AOCLS and SDACCEL ONLY] ON\n"             if($unrollFull);
print $tee "UNROLL_FACTOR          = [AOCLS and SDACCEL ONLY] $unrollFactor\n"  if($unrollFactor);

#AOCL only
print $tee "No-Interleaving?       = [AOCL ONLY] $noInterleaving\n";
print $tee "NUM_SIMD_ITEMS         = [AOCL ONLY] $numSimdItems\n";
print $tee "NUM_COMPUTE_UNITS      = [AOCL ONLY] $numComputeUnits\n";

#SDACCEL only
print $tee "XCL_PIPELINE_WORKITEMS = [SDACCEL ONLY] ON\n" if($xclpipelineWorkitems);
print $tee "XCL_PIPELINE_LOOP      = [SDACCEL ONLY] ON\n" if($xclpipelineLoop);
print $tee "max_memory_ports       = [SDACCEL ONLY] ON\n" if($xclmaxmemoryports);
print $tee "memory_port_data_width = [SDACCEL ONLY] $xclmemportwidth\n" if($xclmemportwidth);

$tee->flush;
close $bplfh;

#--------------------------------------------
#INVALID combination of experiment parameters
#--------------------------------------------
#Dimensions should be divisible by the vector size
if ( ($ROWS % $vectorSize) !=0) {
  die "BUILD.PL: Invalid setup. Vector size must completely divide ROWS.\n";
}

if (($target eq 'AOCL_CHREC') && ($numNovoGdevices!=2)) {
  die "BUILD.PL: Invalid setup. Currently Novo-G target only supports 2 devices.\n";
}


# --------------------------------------------
# >>>> Make a copy of soure files for logging
# --------------------------------------------
my $sourceFilesTarget = $targetDir."/srcCopy";
make_path($sourceFilesTarget);
#system ("cp ../device/Kernel.cl     $sourceFilesTarget") if($ocl);
#todo copy maxeler device files
system ("cp ../host/host.c          $sourceFilesTarget");
system ("cp ../host/host-generic.h  $sourceFilesTarget");
system ("cp ../host/host-generic.c  $sourceFilesTarget");
system ("cp ../host/host-sw2d.c     $sourceFilesTarget");
system ("cp ../host/host-sw2d.h     $sourceFilesTarget");
print "BUILD.PL: Backed up source files in $sourceFilesTarget\n";

#--------------------------------------------
# cd to target dir
#--------------------------------------------
print "BUILD.PL: Moving to target directory...\n";
chdir $targetDir;

# --------------------------------------------------------------------
# create custom .H file to pass macro definitions to kernel compiler 
# --------------------------------------------------------------------

if($ocl) {
  print  "BUILD.PL: creating a custom include file for passing macro defintionst to kernel compiler\n";
  my $kernelCompilerString  = "/*=========================================================================\n"
                            . "Custom kernel include file generated for this run:\n\t"
                            . $targetDir
                            . "\n=========================================================================*/\n"
                            ;
  #first include all enumeration definitions defined in the common header file
  #Note: obvious way would have been to simply include the common header file in the kernel code, if only!
  # different opencl compilers seem to read relative paths to include files differently, so I have taken this route
  my $handle;
  my $enumerationsHeaderFile = "../../common/enumerations.h";
  open $handle, '<', $enumerationsHeaderFile;
  chomp(my @lines = <$handle>);
  close $handle;
  $kernelCompilerString = $kernelCompilerString.join("\n", @lines);
  
  #now append custom parameter definition for this run
  $kernelCompilerString = $kernelCompilerString."\n\n"   
                   ." //===============================================\n"
                   ." // Custom parameter definitions for this run \n"
                   ." //===============================================\n"
                   ." #define TARGET                  $target     \n"
                   ." #define WORD                    $wordType   \n"
                   ." #define VECTOR_SIZE             $vectorSize \n"
                   ." #define ROWS                    $ROWS \n"
                   ." #define COLS                    $COLS \n"
                   ." #define NTOT                    $NTOT \n"
                   ." #define NTIMES                  $NTIMES \n"
                   ." #define WORKITEMS               $workitems \n"
                   ." #define NUM_SIMD_ITEMS          $numSimdItems   \n"
                   ." #define NUM_COMPUTE_UNITS       $numComputeUnits\n"
                   ." #define USECHANNELS             $useChannels    \n"
                   ." \n\n"
                   ;
                   
  #Number of devices, if target is NOVO-G
  $kernelCompilerString = $kernelCompilerString." #define NDEVICES $numNovoGdevices\n" if($target eq 'AOCL_CHREC');
  
  #required workgroup size if defined
  $kernelCompilerString = $kernelCompilerString." #define REQ_WORKGROUP_SIZE $reqWorkgroupSize\n" if($reqWorkgroupSize);
  
  #unroll factor append if relevant
  $kernelCompilerString = $kernelCompilerString."#define UNROLL_FACTOR UNROLL_FULL\n"   if($unrollFull);
  $kernelCompilerString = $kernelCompilerString."#define UNROLL_FACTOR $unrollFactor\n" if($unrollFactor);
  
  #SDACCEL: Pipeline Workitems/loop
  $kernelCompilerString = $kernelCompilerString."#define XCL_PIPELINE_WORKITEMS\n"  if($xclpipelineWorkitems);
  $kernelCompilerString = $kernelCompilerString."#define XCL_PIPELINE_LOOP\n"       if($xclpipelineLoop);
  
  #now append the derived parameter defintions from the common file used in both host and device
  my $derivedParamsFile = "../../common/derivedParameters.h";
  open $handle, '<', $derivedParamsFile;
  chomp(@lines = <$handle>);
  close $handle;
  $kernelCompilerString = $kernelCompilerString.join("\n", @lines);
  
  
  #-----------------------------------------------
  #if target is novo-g, then we need to define NDEVICES, and 
  #generate multiple include (and kernel) files
  #-----------------------------------------------
  if($target eq 'AOCL_CHREC') {
    my $kernelCompilerStringBase = $kernelCompilerString; #need a base copy to make different versions
    for (my $i = 0; $i < $numNovoGdevices; $i++)  {
      $kernelCompilerString = $kernelCompilerStringBase."\n #define MYDEVICEID ".$i."\n";
      my $kstringfile = "kernelCompilerInclude.h";
      my $kstring;
      unless (open $kstring, '>', $kstringfile) {
        die "\nUnable to create $kstringfile\n";    
      } 
      print $kstring "$kernelCompilerString";
      close $kstring;
      system("cp -f $kstringfile ../../device");
      print  "BUILD.PL: custom kernel include file $kstringfile created\n";
      
      # Pass KERNEL source through the pre-processor
      # =============================================================================================
      my $originalKernelFile = "../../device/$kernelSourceFile";
      my $preProcessedKernelFile = "../../device/Kernel_".$i.".cl";
      
      #run pre-processor
      system("cpp -I. -P $originalKernelFile -o $preProcessedKernelFile");
      
      #save generated kernel file in source code backup folder
      system ("cp $preProcessedKernelFile ./srcCopy");
      
    }
  }
  
  #-----------------------------------------------
  #target not novo-g (single device)
  #-----------------------------------------------
  else {
    #now write to file
    my $kstringfile = "kernelCompilerInclude.h";
    my $kstring;
    unless (open $kstring, '>', $kstringfile) {
      die "\nUnable to create $kstringfile\n";    
    }
    print $kstring "$kernelCompilerString";
    close $kstring;
    system("cp -f $kstringfile ../../device");
    print  "BUILD.PL: custom kernel include file $kstringfile created\n";
    
    # Pass KERNEL source through the pre-processor
    # =============================================================================================
    my $originalKernelFile = "../../device/$kernelSourceFile";
    my $preProcessedKernelFile = "../../device/Kernel.cl";
    
    #run pre-processor
    system("cpp -I. -P $originalKernelFile -o $preProcessedKernelFile");
    
    #save generated kernel file in source code backup folder
    system ("cp $preProcessedKernelFile ./srcCopy");
  }
}#if(ocl)

# =============================================================================================
# TARGET-SPECIFIC BUILD
# =============================================================================================

#-----------------------------------------------------------
# Host CPU (no opencl involved)
#-----------------------------------------------------------
if ($target eq 'HOST') {
    my $makeString  = "make"
                    ." TARGET=$target"
                    ." WORD=$wordType"
                    ." ROWS=$ROWS"
                    ." COLS=$COLS"
                    ." NTOT=$NTOT"
                    ." NTIMES=$NTIMES"
                    ." WORKITEMS=$workitems"
                    ." $make"
                    ;
                    
  $makeString = $makeString." LOGRESULTS=$logresults" if($logresults);
  $makeString = $makeString." USECHANNELS=$useChannels" if($useChannels);

  #required workgroup size if defined
  #$makeString = $makeString." REQ_WORKGROUP_SIZE=$reqWorkgroupSize" if($reqWorkgroupSize);

  print  "BUILD.PL: Calling makefile like this: ".$makeString."\n\n";
  system ("cp ../Makefile.template .");
  system ("mv Makefile.template Makefile");
  system  ($makeString);
}#if target eq HOST

#-----------------------------------------------------------
# CPU-intel (used as an opencl device)
#-----------------------------------------------------------
elsif ($target eq 'CPU') {
    my $makeString  = "make"
                    ." TARGET=$target"
                    ." WORD=$wordType"
                    ." VECTOR_SIZE=$vectorSize"
                    ." ROWS=$ROWS"
                    ." COLS=$COLS"
                    ." NTOT=$NTOT"
                    ." NTIMES=$NTIMES"
                    ." WORKITEMS=$workitems"
                    ." LOGRESULTS=$logresults"
                    ." $make"
                    ;

  $makeString = $makeString." LOGRESULTS=$logresults" if($logresults);
  $makeString = $makeString." USECHANNELS=$useChannels" if($useChannels);

  #required workgroup size if defined
  $makeString = $makeString." REQ_WORKGROUP_SIZE=$reqWorkgroupSize" if($reqWorkgroupSize);

  print  "BUILD.PL: Calling makefile like this: ".$makeString."\n\n";
  system ("cp ../Makefile.template .");
  system ("mv Makefile.template Makefile");
  system  ($makeString);
}#if target eq CPU

#-----------------------------------------------------------
# GPU-nvidea (as an OpenCL device)
#-----------------------------------------------------------
elsif ($target eq 'GPU') {
    my $makeString  = "make"
                    ." TARGET=$target"
                    ." WORD=$wordType"
                    ." VECTOR_SIZE=$vectorSize"
                    ." ROWS=$ROWS"
                    ." COLS=$COLS"
                    ." NTOT=$NTOT"
                    ." NTIMES=$NTIMES"
                    ." WORKITEMS=$workitems"
                    ." LOGRESULTS=$logresults"
                    ." $make"
                    ;

  $makeString = $makeString." LOGRESULTS=$logresults" if($logresults);
  $makeString = $makeString." USECHANNELS=$useChannels" if($useChannels);

  #required workgroup size if defined
  $makeString = $makeString." REQ_WORKGROUP_SIZE=$reqWorkgroupSize" if($reqWorkgroupSize);

  print  "BUILD.PL: Calling makefile like this: ".$makeString."\n\n";
  system ("cp ../Makefile.template .");
  system ("mv Makefile.template Makefile");
  system  ($makeString);
}#if target eq GPU

#-----------------------------------------------------------
# SDACCEL (TCL) target
#-----------------------------------------------------------
elsif ($target eq 'SDACCEL') {

  # >>>> GENERATE CUSTOM TCL
  # --------------------------------

  #note that unlike other targets, we call TCL file from ABOVE the
  #target build folder. Since we have cd'ed to it, we go one step up here...
  chdir("..");

  # temp string buffer used in file generation 
  my $strBuf = ""; 

  #target TCL file, same name as target build folder
  my $targetTclFilename = $targetDir.".tcl";  
  open(my $tclfh, '>', $targetTclFilename)
    or die "BUILD.PL: Could not open file '$targetTclFilename' $!";  
  
  # >>>> Load TCL build template file
  my $templateFileName = "perl-build.tcl.template"; 
  open (my $fhTemplate, '<', $templateFileName)
    or die "BUILD.PL: Could not open file '$templateFileName' $!"; 

  # >>>> Read template contents into string
  my $genCode = read_file ($fhTemplate);
  close $fhTemplate;

  # >>>>> Update parameter definitions
  $strBuf = "$strBuf"."set ROWS                   $ROWS\n";
  $strBuf = "$strBuf"."set COLS                   $COLS\n";
  $strBuf = "$strBuf"."set NTOT                   $NTOT\n";
  $strBuf = "$strBuf"."set NTIMES                 $NTIMES\n";
  $strBuf = "$strBuf"."set VECTOR_SIZE            $vectorSize\n";
  $strBuf = "$strBuf"."set TARGET                 \"$target\"\n";
  $strBuf = "$strBuf"."set WORD                   \"$wordType\"\n";
  $strBuf = "$strBuf"."set WORKITEMS              \"$workitems\"\n";
  $strBuf = "$strBuf"."set REQ_WORKGROUP_SIZE     \"$reqWorkgroupSize\"\n" if($reqWorkgroupSize);
  $strBuf = "$strBuf"."set FLOW                   \"$FLOW\"\n";
  $strBuf = "$strBuf"."set Make                   \"$make\"\n";
  $strBuf = "$strBuf"."set destDir                \"$targetDir\"\n";
  $strBuf = "$strBuf"."set timeStamp              \"$timeStamp\"\n";
  $genCode =~ s/<params>/$strBuf/g;

  #enable max_memory_ports=true or not
  $strBuf = "";
  $strBuf = "$strBuf"
          ."set_property max_memory_ports true [get_kernels \$kernel]\n" if($xclmaxmemoryports);

  #set memory_port_data_width if specified
  $strBuf = "$strBuf"
          ."set_property memory_port_data_width $xclmemportwidth [get_kernels \$kernel]\n\n" if($xclmemportwidth);


  $genCode =~ s/<max_memory_ports>/$strBuf/g;

  # report estimate or not
  $strBuf = "";
  if($estimate) {
    $strBuf = "$strBuf"."\n# ---------------------------------------------------------\n";
    $strBuf = "$strBuf"."# Generate the system estimate report\n";
    $strBuf = "$strBuf"."# ---------------------------------------------------------\n";
    $strBuf = "$strBuf"."report_estimate\n";
  }
  $genCode =~ s/<estimate>/$strBuf/g;

  # do HW build or not
  $strBuf = "";
  if ($FLOW eq 'hw') {
    $strBuf = "$strBuf"."\n# ---------------------------------------------------------\n";
    $strBuf = "$strBuf"."# Compile the application to run on the accelerator card\n";
    $strBuf = "$strBuf"."# ---------------------------------------------------------\n";
    $strBuf = "$strBuf"."build_system\n";
    $strBuf = "$strBuf"."\n";
    $strBuf = "$strBuf"."# ---------------------------------------------------------\n";
    $strBuf = "$strBuf"."# Package the application binaries\n";
    $strBuf = "$strBuf"."# ---------------------------------------------------------\n";
    $strBuf = "$strBuf"."package_system\n";
  }
  $genCode =~ s/<hw>/$strBuf/g;

  # >>>>> Write to file and make a backup copy
  $genCode =~ s/\r//g; #to remove the ^M  
  print $tclfh $genCode;
  print "BUILD.PL: Generated custom TCL file \n";
  close $tclfh;
  system ("cp $targetTclFilename $sourceFilesTarget");
  
  # call TCL
  # ---------
  system ("sdaccel $targetTclFilename");

  # run if asked
  # -------------
  my $execDir = $targetDir."/pkg/pcie/";
  my $executable = $targetDir.".exe";
  my $xclbin = "streamKernel.xclbin";

  if (($make eq 'run') && ($FLOW eq 'hw')) {
    print "BUILD.PL: Running HW \n";
    chdir "$execDir";
    print "BUILD.PL: cd to target directory $execDir\n";
    system ("./".$executable." ".$xclbin."\n");
    print "BUILD.PL: Running executable on hardware\n";
    chdir "../../../";
    print "BUILD.PL: cd back to parent directory\n";
  }#if
}#if target eq SDACCEL

#-----------------------------------------------------------
# AOCL target (BOLAMA or CHREC)
#-----------------------------------------------------------
elsif (($target eq 'AOCL') || ($target eq 'AOCL_CHREC')) {
  my $makeString = "make"
          ." TARGET=$target"
          ." WORD=$wordType"
          ." NUM_SIMD_ITEMS=$numSimdItems"
          ." NUM_COMPUTE_UNITS=$numComputeUnits"
          ." NO_INTERLEAVING=$noInterleaving"
          ." ROWS=$ROWS"
          ." COLS=$COLS"
          ." NTOT=$NTOT"
          ." NTIMES=$NTIMES"
          ." WORKITEMS=$workitems"
          ." LOGRESULTS=$logresults"
          ." FLOW=$FLOW"
          ." NDEVICES=$numNovoGdevices" 
          ." $make"
          ;

  $makeString = $makeString." LOGRESULTS=$logresults" if($logresults);
  $makeString = $makeString." USECHANNELS=$useChannels" if($useChannels);

  #required workgroup size if defined
  $makeString = $makeString." REQ_WORKGROUP_SIZE=$reqWorkgroupSize" if($reqWorkgroupSize);

  print   "BUILD.PL: Calling makefile like this: ".$makeString."\n\n";
  system ("cp ../Makefile.template .");
  system ("mv Makefile.template Makefile");
  #system ("cp ../Makefile .");
  system ($makeString);
}#if target eq AOCL

#-----------------------------------------------------------
# DIE
#-----------------------------------------------------------
else
  {die "Invalid TARGET specificied >> $target";}

# =============================================================================================
# POST PROCESSING
# =============================================================================================
 
#my $end = Time::HiRes::gettimeofday();
#printf("\nBUILD.PL: The build type <%s> took %.2f seconds\n",$make,  $end - $start);
printf("\nBUILD.PL: Build files have been saved in $targetDir\n");
printf("==================================================================\n");
printf("BUILD.PL: ---- BUILD COMPLETE ----\n",$make);
printf("==================================================================\n");
printf("\n\n\n\n");

# ----------------------------------------------------------------------------
#
# ----------------------------------------------------------------------------
sub printHelp {
  print "\nOpenCL stream Benchmark build script command line options and defaults (*)\n";
  print "--------------------------------------------------------------------------------------\n";
  print "-tar[HOST/AOCL/AOCL_CHREC/SDACCEL/GPU/CPU/]<*>  : Target platform. MUST be specified.\n";
  print "-w  [INT/FLOAT*/DOUBLE]                         : data-type of WORD used in the streams \n";
  print "-row[53]                                        : ROWS in the 2D array\n";
  print "-col[53]                                        : COLS in the 2D array\n";
  print "-ntot[1000]                                     : how many time steps?\n";
  print "-ntimes[10]                                     : how many simulatiom runs?\n";
  print "-r  [NULL*]                                     : Value for attribute <reqd_work_group_size>\n";
  print "-wi [*SINGLEWI/NDRANGE]                         : ndrange or single work-item kernel\n";
  print "-v  [1]                                         : size of vector for kernel streams (1 = scalar) \n";
  print "-m  [all*/host/aoclbin/run/clean]               : type of make\n";
  print "-log                                            : do you want to log results to file?\n";
  print "-h                                              : print help \n";
  print "-nut [undef]                                    : if defined, *no* unique target directory created for this experiment\n";
  #print "\n";
  #print "-batch                             : Run a batch job. List of jobs ALWAYS picked from jobs.bat file in THIS folder.\n";
  #print "\n";

  print "\nAOCL+SDACCEL specific parameters\n";
  print "----------------------------------\n";
  print "-nd                                : Number of devices (only for AOCL_CHREC target).\n";
  print "-ch [undef]                        : Use AOCL channels between kernels\n";
  print "-f  [cpu_emu*/hw_emu/hw]           : build flow\n";
  print "-uf [undefined]                    : should loop be FULLY  unrolled(only for -l = KERNEL)\n";
  print "-un N                              : should loop be unrolled by a factor of N? (only for -l = KERNEL)\n";
  
  print "\nAOCL specific parameters\n";
  print "--------------------------\n";
  print "-s  [1]                            : AOCL optimization attribute <num_simd_work_items>\n";
  print "-c  1                              : AOCL optimization attribute <num_compute_units>\n";
  print "-ni [undef]                        : No-Interleaving of (default) Global Memory?\n";

  print "\nSDACCEL specific parameters\n";
  print "-----------------------------\n";
  print "-e  [undef*]                       : include if you want to call \"report_estimate\".  \n";
  print "                                     turned in automatically if flow = hw              \n";
  print "-xpw [undef*]                      : turn on XCL_PIPELINE_WORKITEMS\n";
  print "-xpl [undef*]                      : turn on XCL_PIPELINE_LOOP\n";
  print "-xmm [undef*]                      : enable sdaccel optimization max_memory_ports\n";
  print "-xmw [undef*]                      : value for xcl opt' param' <memory_port_data_width>\n";

  print "\nGPU specific parameters\n";
  print "-------------------------\n";

  print "\nCPU specific parameters\n";
  print "-------------------------\n";


  print "\nUsage example\n";
  print "-------------\n";
  print "./build.pl -tar HOST -w FLOAT -row 53 -cokl 53\tOR\n";
  print "perl build.pl <flags>\n";
  print "\nNotes\n";
  print "--------\n";
  print "1. If parameters not relevant to target are specified, they are ignored. No errors or warnings are raised\n";
  print "2. <*> indicates REQUIRED option \n";

}
