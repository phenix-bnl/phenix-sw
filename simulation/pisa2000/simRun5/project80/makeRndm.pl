#!/usr/local/bin/perl

#
# PERL script to copy PISA input files from PHENIX AFS area and to create simultaneous job subdirectories
#
# Usage  perl -w makeRndm.pl -njobs=N -magfield=mapString -rhicrun=runString
#
#        valid magnetic field map descriptors for mapString
#        Sim3D01   Run1/Run2 magnetic field map (no North Muon Arm)
#        Sim3D03   Run3/Run4 magnetic field map with North Muon Arm but only outer coil
#        Sim3D++   Run4 magnetic field map with both inner and outer coils
#        Sim3D+-   Future Run magnetic field map with reversed inner and outer coils
#
#        valid runString values for copying from /afs/rhic/phenix/software/simulation/runString
#        run2b  (for 200 GeV AuAu) 
#        run3a  (for 200 GeV pp)
#        run3b  (for 200 GeV dAu)
#        run4a  (for 200 GeV AuAu)
#        run4b  (for 62.4 GeV AuAu)
#        run5a  (all combinations)
#
#

use strict;
use Getopt::Long;

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

my $numberStart = 1;
my $numberEnd;
my $rndmBase0 = "rndm0";
my $rndmBase = "rndm";
my $number;
my $rndmxx;
my $njobs;
my $magfield;
my $rhicrun;

GetOptions("njobs=s" => \$njobs, "magfield=s" => \$magfield, "rhicrun=s" => \$rhicrun);

if($njobs) {
    print "\n  Number of simultaneous jobs to be run = $njobs\n";
    $numberEnd = $njobs;
}
else {
    die "Input parameter -njobs is missing";
}

GetOptions("njobs=s" => \$njobs, "magfield=s" => \$magfield);

if($magfield) {
    print "\n  Magnetic field map = $magfield\n";
}
else {
    die "Input parameter -magfield is missing";
}

if($rhicrun) {
    print "\n  RHIC run = $rhicrun\n";
}
else {
    die "Input parameter -rhicrun is missing";
}

#
# Copy standard PISA input files to top directory
#
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/event.par .`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/flukaaf.dat .`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/gffgo.dat .`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/glogon.kumac .`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/phnx.par .`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/pisa.kumac .`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$rhicrun/xsneut95.dat . &`;
`cp /afs/rhic.bnl.gov/phenix/software/simulation/$magfield.root . &`;

#
# dummy oscar.root file initially
#
`touch oscar.root` ;

mkdir("done", 0775);

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
#   softlinks to the PISA input files in the top project directory
#
    `ln -fs ../event.par .`;
    `ln -fs ../flukaaf.dat .`;
    `ln -fs ../gffgo.dat .`;
    `ln -fs ../glogon.kumac .`;
    `ln -fs ../oscar.root .`;
    `ln -fs ../phnx.par .`;
    `ln -fs ../pisa.kumac .`;
    `ln -fs ../xsneut95.dat .`;

    `ln -fs ../$magfield.root .`
    
} # loop over numbers

print "\n\n Finished makeRndm.pl\n\n";

exit;
