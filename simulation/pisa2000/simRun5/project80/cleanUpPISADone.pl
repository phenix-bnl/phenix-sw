#!/usr/local/bin/perl

use strict;
use Getopt::Long;

# PERL script to check for bad (too small) PISA files
#
# Usage  perl -w cleanUpPISADone.pl -nRndm=N
#

use File::stat;
my $fileSize;
my $info;

#
# You need to change the topName location for your project
#
my $topName = "/scratch/maguire/0/project80/pisaEvents";
if(-e $topName) {
    chdir $topName;
    print "\n\n   Using top directory $topName\n";
}
else {
    print "\n\n Unable to locate top directory $topName\n";
    print "    Exiting without further checking or submitting any jobs\n";
    exit;
} # safety check on existence of top directory


my $nRndm;

GetOptions("nRndm=s" => \$nRndm);
if($nRndm) {
    print "\n  Number of directories to be cleaned = $nRndm\n";
}
else {
    die "Input parameter -nRndm is missing";
}

my $number;
my $rndmBase = "rndm";
my $rndmBase0 = "rndm0";
my $rndmxx;

for($number=0; $number<$nRndm; $number++) {
    if($number<10) {
	$rndmxx = $rndmBase0 . $number;
    }
    if($number>9) {
	$rndmxx = $rndmBase . $number;
    }
    if(-e $rndmxx) {
	chdir $rndmxx;
	`ls -c1 pisa*.out > pisaOut.txt`;
	open(INPUT, "pisaOut.txt");
	while(<INPUT>) {
	    my $fileName = $_;
	    chomp($fileName);
	    $info = stat($fileName);
	    $fileSize = $info->size;
	    if($fileSize < 9999) {
		print "\n Small file size $fileSize";
	    }
	    
	}
    }
    print "\n finished directory $rndmxx\n";

}
exit;
