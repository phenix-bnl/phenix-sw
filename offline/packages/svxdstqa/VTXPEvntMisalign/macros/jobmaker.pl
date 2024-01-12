#!/usr/bin/perl

use strict;
use warnings;

open (my $runlist, '<', 'runlist_misaligned.txt') or die "Could not open file $!";
while( <$runlist> ) 
{
    chomp($_);
    # $. is line number
    # $_ is line content
    my $fn = sprintf("misalign_%d.job",$_);
    open (my $fh, '>', $fn) or die "Could not open file $fn: $!";

    print $fh "Universe        = vanilla\n";
    print $fh "Notification    = Error\n";
    print $fh "Executable      = run_search_misaligned.csh\n";
    print $fh "Arguments       = $_ \$(Process)\n";
    print $fh "Requirements    = (CPU_Speed >= 1 && TotalDisk > 0)\n";
    print $fh "Rank            = CPU_Speed\n";
    print $fh "Image_Size      = 1000M\n";
    print $fh "Priority        = +20\n";
    print $fh "GetEnv          = True\n";
    print $fh "Initialdir      = /direct/phenix+u/zrowan/offline/packages/svxdstqa/VTXPEvntMisalign/macros/misaligned\n";
    print $fh "Input           = /dev/null\n";
    print $fh "Output          = /gpfs02/phenix/vtx/subsys/vtx/zrowan/misaligncheck_misaligned/stdout/out_$_.\$(Process)\n";
    print $fh "Error           = /gpfs02/phenix/vtx/subsys/vtx/zrowan/misaligncheck_misaligned/stderr/err_$_.\$(Process)\n";
    print $fh "Log             = /gpfs02/phenix/vtx/subsys/vtx/zrowan/misaligncheck_misaligned/log/log_$_.\$(Process)\n";
    print $fh "Notify_user     = username\@bnl.gov\n";
    print $fh "+Experiment     = \"phenix\"\n";
    print $fh "+Job_Type       = \"cas\"\n";
    print $fh "Queue 30\n";

    close $fh;
    
}
close $runlist;
print "done\n";
