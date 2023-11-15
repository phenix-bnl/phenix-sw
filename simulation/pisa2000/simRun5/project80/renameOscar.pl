#!/usr/local/bin/perl
 
# Script to rename OSCAR files for Project80 to RunServer numbers
#
# Usage:             perl -w renameOscar.pl >& renameOscar.log & 
#
use strict;

my $topName = "/scratch/maguire/0/project80/exodusEvents/";
my $originalList = $topName . "originalList.txt";
my $newList = $topName . "newList.txt";
my $runNumber = 80701;
my $project = 80;

if(-e $topName) {
    chdir $topName;
    `ls -c1 *.oscar > $originalList`;
}
else {
    die "No top name directory $topName";
}

my $preName = "oscar_om3p_dgh_";
my $endName = "_00" . $project . ".ascii";
my $originalName;
my $newName;
my $fileCount = 0;

open(INPUT, "$originalList");
open(OUTPUT, ">$newList");
while(<INPUT>) {
    chop($_);
    $originalName = $_;
    $newName = $preName . $runNumber .$endName;
    print OUTPUT " Old file $originalName renamed to $newName\n";
    `mv $originalName $newName`;
    $runNumber++;
    $fileCount++;
}

close(INPUT);
close(OUTPUT);

print "\n\n   renameOscar.pl completed for $fileCount files\n";
__END__

