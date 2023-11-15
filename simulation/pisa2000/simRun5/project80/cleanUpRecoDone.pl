#!/usr/local/bin/perl

use strict;
use Getopt::Long;

# PERL script to copy PISA input files from PHENIX AFS area and to create simultaneous job subdirectories
#
# Usage  perl -w cleanUpRecoDone.pl
#

use File::stat;
my $fileSize;
my $info;

#
# You need to change the topName location for your project
#
my $topName = "/scratch/maguire/0/project80/dstEvents/done";
if(-e $topName) {
    chdir $topName;
    print "\n\n   Using top directory $topName\n";
}
else {
    print "\n\n Unable to locate top directory $topName\n";
    print "    Exiting without further checking or submitting any jobs\n";
    exit;
} # safety check on existence of top directory

opendir(DIR,"/scratch/maguire/0/project80/dstEvents/done") || die "\n Can't open: $!\n";
my @parentFiles = readdir(DIR);
closedir(DIR);

my $fileName;
my $nFile = 0;
my $nKeep = 0;
my $nRemove = 0;
foreach $fileName (@parentFiles) {
    $nFile++;
    if ($nFile> 3) {
	$info = stat($fileName);
	$fileSize = $info->size;
	if($fileSize <1000) {
	    $nRemove++;
	    unlink("$fileName")|| die "\ failure to remove $fileName\n";;
	}
	else {
	    $nKeep++;
	}
    }
}

print "\n Kept $nKeep ,  removed $nRemove\n";

exit;
