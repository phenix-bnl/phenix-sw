#!/usr/local/bin/perl
#
# PERL script to copy Project 80 files from RCF to ACCRE
#
# Usage  perl -w  gridCopyProject80.pl (runs only on firebird node)
#
use strict;

my $accreDirectory = "/gpfs1/scratch/maguire/0/project80/OSCAR_FILES/";
my $fileList = "project80NewOscar.txt";
my $rcfDirectory = "gsiftp://rftpexp.rhic.bnl.gov/phenix/data11/riabovvg/OSCAR_FILES/OM_PI0_PIP_PIM/";

if(-e $accreDirectory) {
    chdir $accreDirectory;
}
else {
    die "\n no accre directory $accreDirectory\n";

 
}
if(-e $fileList) {
    print "\n Using file list $fileList\n"
}
else {
    die "\n Cannot find file list $fileList\n";
}

my $gridCopy = $accreDirectory . "gridCopy.csh";
my $gridCopyLog = $accreDirectory . "gridCopy.log";

open(GRIDCOPY, ">$gridCopy");
print GRIDCOPY "#! /bin/csh\n";
print GRIDCOPY "source /accre/vdt/setup.csh\n";

open(INPUTLIST, "project80NewOscar.txt");
while(<INPUTLIST>) {
    my $fileName = $_;
    chop($fileName);
    my $rcfName = $rcfDirectory . $fileName;
    my $accreName = "gsiftp://vampire.accre.vanderbilt.edu:2811" . $accreDirectory . $fileName;
    print GRIDCOPY "globus-url-copy -p 20 $rcfName $accreName\n";
}

close(GRIDCOPY);
`chmod +x $gridCopy`;
`$gridCopy >& $gridCopyLog`;

__END__
