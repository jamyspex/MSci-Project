#!/usr/bin/env perl
use v5.24;
use warnings;
use strict;

my %nd = (
 nested_grid_start_x => 100,
 nested_grid_start_y => 100,
 nested_grid_x => 200,
 nested_grid_y => 200,
 orig_grid_x => 300,
 orig_grid_y => 300,
 dxgrid_nest => 2,
 dygrid_nest => 2,
 dxgrid_orig => 4,
 dygrid_orig => 4,
);

my $VIS=0;

if (not @ARGV) {
    die "$0 GIS_file\n";
}
my $gis_file = $ARGV[0];
my $file_name = $gis_file;
$file_name=~s/\.txt$//;

my $row_size = $nd{orig_grid_x};

open my $IN, '<', $gis_file or die $!;

my $domain=[];
my $row_ct = 0;
my $col_ct = 0;
my $push_count=0;

while (my $val = <$IN>) {
    chomp $val;
    if ($col_ct==0) {
        $domain->[$row_ct]=[];        
    }
    if ($col_ct == $row_size) {
        $col_ct=0;
        $row_ct++;
    }
#    say "$row_ct, $col_ct, $val";
    push @{$domain->[$row_ct]}, $val;
    $col_ct++;
}
close $IN;

my $n_rows = $row_ct+1;

output_dat_file_and_plot($domain) if $VIS;
# Now manipulate this



my $ratio_x = $nd{dxgrid_orig} / $nd{dxgrid_nest};
my $ratio_y = $nd{dygrid_orig} / $nd{dygrid_nest};

# First increase the columns
my $ndomain=[];
for my $row (0 .. $n_rows-1) {
    $ndomain->[$row]=[];
    for my $col (0 .. $row_size-1) { 
            my $val =  $domain->[$row][$col];
        if ($col >= $nd{nested_grid_start_x} && $col < $nd{nested_grid_start_x} + $nd{nested_grid_x}/$ratio_x) {
#            $domain->[$row][$col]=[$val,$val];
            @{$ndomain->[$row]} = (@{$ndomain->[$row]}, $val, $val);
        } else {
            push @{$ndomain->[$row]}, $val;
        }
    }
}

output_dat_file_and_plot($ndomain) if $VIS;

my $nndomain=[];

for my $row (0 .. $n_rows -1) {
    push @{$nndomain}, $ndomain->[$row];
        if ($row >= $nd{nested_grid_start_y} && $row < $nd{nested_grid_start_y} + $nd{nested_grid_y}/$ratio_y) {
            push @{$nndomain}, $ndomain->[$row];
        } 
}

output_dat_file_and_plot($nndomain) if $VIS;

output_new_GIS($nndomain, \%nd);

sub output_new_GIS { (my $domain, my $nest_def) =@_;
    my @ks = sort keys %{$nest_def};
    my $nest_str= join('_',map { $nest_def->{$_} } @ks);
    open my $GIS,'>', $file_name.'_nest_'.$nest_str.'.txt' or die $!;
    my $n_rows = scalar @{$domain};
    my $n_cols = scalar @{$domain->[0]};
    for my $row (0 .. $n_rows-1) {
        for my $col (0 .. $n_cols-1) { 
            my $val =  $domain->[$row][$col];
            say $GIS $val;
        }
    }
    close $GIS;   
}

sub output_dat_file_and_plot { (my $domain) =@_;
    open my $DAT, '>', $file_name.'.dat' or die $!;
    my $n_rows = scalar @{$domain};
    my $n_cols = scalar @{$domain->[0]};
    for my $row (0 .. $n_rows-1) {
        for my $col (0 .. $n_cols-1) { 
            my $val = $domain->[$row][$col];
            if (ref($val) eq 'ARRAY') {
                $val = join("\t",@{$val});
            }
            print $DAT $val,"\t";
        }
        print $DAT "\n";
    }
    close $DAT;
    open my $GP, '>','tmp.gnuplot';
    say $GP "plot '$file_name.dat' matrix with image";
    close $GP;
    system("gnuplot tmp.gnuplot");
}
