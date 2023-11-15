#!/usr/local/bin/perl

#
# PERL script to create directores for simultaneous Pisa-to-DST
#
# Usage  perl -w nanoMakeRndm.pl -njobs=N -rhicrun=runString
#
#        valid runString values for copying from /afs/rhic/phenix/software/simulation/runString
#        run2b (200 GeV AuAu)
#        run3a (200 GeV pp)
#        run3b (200 GeV dAu)
#        run4a (200 GeV AuAu)
#        run4b (62.4 GeV AuAu)
#        run5a (all combinations)
#        head (present CVS head directory)
#

use strict;
use Getopt::Long;

#
# You must change this topName to point to your own working directory
#
my $topName = "/scratch/maguire/0/project93/dstEvents/PiPlus/PosField";
if(-e $topName) {
    chdir $topName;
    print "\n\n  Using top directory $topName\n\n";
}
else {
    print "\n\n Unable to locate top directory $topName\n";
    print "    Exiting without further checking or submitting any jobs\n";
    exit;
} # safety check on existence of top directory

#
# No need to change anything else in this PERL script
#

#
# These versions can be edited (if needed but usually not) in the local directory
#
`cp  /home/phenix/macroTest/pisaToDSTRun5.C pisaToDST.C`;
`cp  /home/phenix/macroTest/pisaToDST_IOManager.C .`;

mkdir("done", 0775);

#
# The following do not need to be changed
#
my $numberStart = 1;
my $numberEnd;
my $rndmBase0 = "rndm0";
my $rndmBase = "rndm";
my $number;
my $rndmxx;
my $njobs;
my $rhicrun;

GetOptions("njobs=s" => \$njobs, "rhicrun=s" => \$rhicrun);

if($njobs) {
    print "\n  Number of simultaneous jobs to be run = $njobs\n";
    $numberEnd = $njobs;
}
else {
    die "Input parameter -njobs is missing";
}

if($rhicrun) {
    print "\n  RHIC run = $rhicrun\n";
}
else {
    die "Input parameter -rhicrun is missing";
}

#
# Create rndmxx areas
#

for($number=$numberStart; $number<=$numberEnd; $number++) {
    chdir $topName;
  if($number<10) {
    $rndmxx = $rndmBase0 . $number;
  }
  if($number>9) {
    $rndmxx = $rndmBase . $number;
  }
    mkdir($rndmxx, 0775);
    chdir $rndmxx;
    print "\n  Creating subdirectory $rndmxx";
#
#   Main control file for Fun4All (from parent directory)
#
    `ln -fs ../pisaToDST.C .`;
    `ln -fs ../pisaToDST_IOManager.C .`;
#
#   Drift Chamber geometry files formerly on AFS
#
    `ln -fs /home/phenix/afsFiles/DchEfficiency_AuAu63.Real .`;
    `ln -fs /home/phenix/afsFiles/DchGeometry.info .`;
    `ln -fs /home/phenix/afsFiles/DchGeometry.wireMc .`;
    `ln -fs /home/phenix/afsFiles/DchGeometry.frame00NoRetracted .`;

#
#   RICH mapping file formerly on AFS
#
    `ln -fs /home/phenix/afsFiles/crk_cabling_vrdc.txt .`;

#
#   softlinks to input files in the top project directory according to rhicrun value
#
    if($rhicrun eq "head") {
	`ln -fs /home/phenix/afsFiles/fieldIntegral++.dat.run04 fieldIntegral.dat`;  # 3D++ field map
	`ln -fs /home/phenix/afsFiles/AlwaysDeadCh_pp200_168676.dat .`;
	`ln -fs /home/phenix/afsFiles/DchEfficiency_pp200.Real .`;
    }

    if($rhicrun eq "run5a") {
	`ln -fs /home/phenix/afsFiles/fieldIntegral++.dat.run04 fieldIntegral.dat`;  # 3D++ field map
	`ln -fs /home/phenix/afsFiles/AlwaysDeadCh_pp200_168676.dat .`;
	`ln -fs /home/phenix/afsFiles/DchEfficiency_pp200.Real .`;
    }

    if($rhicrun eq "run4a") {
	`ln -fs /home/phenix/afsFiles/fieldIntegral++.dat.run04 fieldIntegral.dat`;  # 3D++ field map
	`ln -fs /home/phenix/afsFiles/AlwaysDeadCh_AuAu_Run04.dat .`;
	`ln -fs /home/phenix/afsFiles/DchEfficiency_AuAu63.Real .`;  # actually a constant 95%
    }

    if($rhicrun eq "run4b") {
	`ln -fs /home/phenix/afsFiles/fieldIntegral++.dat.run04 fieldIntegral.dat`;  # 3D++ field map
	`ln -fs /home/phenix/afsFiles/AlwaysDeadCh_AuAu63_122929.dat .`;
	`ln -fs /home/phenix/afsFiles/DchEfficiency_AuAu63.Real . ;` # actually a constant 95%
    }
    
    if($rhicrun eq "run3a") {
	`ln -fs /home/phenix/afsFiles/fieldIntegral.dat.run03 fieldIntegral.dat`;  # 3D03 field map
	`ln -fs /home/phenix/afsFiles/AlwaysDeadCh_pp_Run03.dat .`;
	`ln -fs /home/phenix/afsFiles/DchEfficiency_AuAu63.Real .` ; # actually a constant 95%	
    }
    
    if($rhicrun eq "run3b") {
	`ln -fs /home/phenix/afsFiles/fieldIntegral.dat.run03 fieldIntegral.dat`;  # 3D03 field map
	`ln -fs /home/phenix/afsFiles/AlwaysDeadCh_dAu_72096.dat .`;
	`ln -fs /home/phenix/afsFiles/DchEfficiency_dAu.Real .` ; # actually a constant 95%	
    }
    
} # loop over numbers

print "\n\n Finished nanoMakeRndm.pl\n\n";

exit;
