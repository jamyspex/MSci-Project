#!/usr/bin/env perl
use warnings;
use strict;
use v5.10;
BEGIN {
	push @INC, '../aux';
};
use MacroFileToCmdLine qw( macro_file_to_cmd_line_str );

my $macros_str= macro_file_to_cmd_line_str( './macros.h', '-D');
my $macros_to_skip_str = macro_file_to_cmd_line_str('./macros_to_skip.h','-X');

#if ( (not -e $macros_src || ! -e $macros_to_skip_src) && @ARGV<2) {
#    die "Please specify the source for the macros to be defined and skipped\n";
#}

#open my $MK, '<', $macros_src or die $!;
#my @ls=();
#while (my $line=<$MK>) {
#    chomp $line;
#    $line=~s/^\s*//;
#    $line=~s/\s*$//;
#    $line=~s/\#define\s*//;
#    $line=~/\#undef/ && next;
#    $line=~s/\s+(\d+)/=$1/;
#    push @ls, $line;
#}
#close $MK;
#my $macros_str=join(" ",@ls);
#if ($macros_str ne '') {
#$macros_str = '-D '.$macros_str;
#}
#open my $MKS, '<', $macros_to_skip_src or die $!;
#my @lss=();
#while (my $line=<$MKS>) {
#    chomp $line;
#    $line=~s/^\s*//;
#    $line=~s/\s*$//;
#    $line=~s/\#define\s*//;
#    $line=~/\#undef/ && next;
#    $line=~s/\s+(\d+)/=$1/;
#    push @lss, $line;
#}
#close $MKS;
#my $macros_to_skip_str=join(" ",@lss);
#if ($macros_to_skip_str ne '') {
#$macros_to_skip_str = '-X '.$macros_to_skip_str;
#}
say("AutoParallel-Fortran-exe ./adam.f95 ./bondv1.f95 ./feedbf.f95 ./les.f95 ./press.f95 ./velFG.f95 ./velnw.f95 -out ../GeneratedCodeV2/ -iowrite anime -main ./main.f95 -plat GPU  $macros_str $macros_to_skip_str -v");
#system('which AutoParallel-Fortran-exe');die;
system("AutoParallel-Fortran-exe ./adam.f95 ./bondv1.f95 ./feedbf.f95 ./les.f95 ./press.f95 ./velFG.f95 ./velnw.f95 -out ../GeneratedCodeV2/ -iowrite anime -main ./main.f95  -plat GPU  $macros_str $macros_to_skip_str -v");


