#!/usr/bin/env perl

my @files=glob('data*.dat');

my $h = 0;
my $m = 0;
my $d = 1;
my $mm = 1;
my $y = 2000;
for my $y  (2000 .. 2017) {
for my $mm (1 .. 12) {
    if ($mm<10) {$mm='0'.$mm}
for my $d  (1 .. 31) {
    if ($d<10) {$d='0'.$d}
for my $h  (0 .. 23) {
    if ($h<10) {$h='0'.$h}
for my $m  (0 .. 59) {
    my $f = shift @files;
    if ($m<10) {$m='0'.$m}
    my $date_str = $y.$mm.$d.'_'.$h.$m;
    system("ln -s $f $date_str\n");
    exit() if scalar( @files) ==0;
}}}}}
