#!/usr/bin/env perl
use v5.22;
use warnings;
use strict;
use Data::Dumper;
use Cwd;
my $wd=cwd();

if (!@ARGV) {
    die "$0 path-to-stash-file (probably ../src/stash.pl)\n";
}

my $VV=1;


replace_stash($ARGV[0]);

sub replace_stash { (my $stash_src)=@_;


    if (not -e $stash_src) {
        die "Could not fine $stash_src\n";    
    }
    if (not -d './PostGen') {
    mkdir './PostGen';
    }

    my $stash_ref = do( $stash_src );

    for my $src (keys %{$stash_ref}) {
        my @out_lines=();
        my $src_file= $src;
        if (not -e $src_file) {
            $src_file=~s/\.f95/_host.f95/;
        }
#        say $src_file;
        open my $IN, '<', $src_file or die $!;

        while (my $line = <$IN>) {
            chomp $line;
            #           say $line;
            if ($line=~/^\s+(\d+)\s+continue/) {
                my $tag=$1;
                if (exists $stash_ref->{$src}{$tag} ) {
                    for my $stashed_line (@{  $stash_ref->{$src}{$tag} }) {
                        push @out_lines, $stashed_line;
                    }
                } else {
                    push @out_lines, $line;
                }
            } else {
                push @out_lines, $line;
            }
        }
        close $IN;
        open my $OUT, '>', 'PostGen/'.$src_file or die $!;
        map {say $OUT $_} @out_lines;
        close $OUT;
    }
}


#map {say $_ } @{  $ref->{'main.f95'}{7188} };


