#!/usr/local/bin/perl

#
# PERL script to copy DST simulation project to RCF
#      Using gridFTP copy (globus-url-copy) with Maguire certificate
#      Current proxy good until Friday, June 23 (2006)
#
# Usage  perl -w gridCopyDST.pl  (runs only on firebird node)
#
use strict;
use File::stat;

my $topDirectory = "/scratch/maguire/0/project80/";
my $destDir = "gsiftp://rftpexp.rhic.bnl.gov/phenix/data11/maguire/project80/dstEvents/";
my $sourceDir = $topDirectory . "dstEvents/done/";

my $gridCopy = "gridCopyDST";

#
# if testMode = 1, then the copy script is not executed
#
my $testMode = 0;

my $sec;
my $min;
my $hour;
my $monthday;
my $month;
my $year;
my $weekday;
my $yearday;
my $isdaylight;

my %dayWeek = ("0"=>"Sunday","1"=>"Monday", "2"=>"Tuesday","3"=>"Wednesday",
               "4"=>"Thursday","5"=>"Friday", "6"=>"Saturday");

my %monthYear = ("0"=>"January","1"=>"February", "2"=>"March","3"=>"April",
                 "4"=>"May","5"=>"June", "6"=>"July", "7"=>"August",
                 "8"=>"September","9"=>"October", "10"=>"November", "11"=>"December");

($sec, $min, $hour, $monthday, $month, $year, $weekday, $yearday, $isdaylight) = localtime;
my $trueYear = 1900 + $year;

my @pisaFileList;
if(-e $topDirectory) {
    chdir $topDirectory;
    print "\n\n   Using top directory $topDirectory\n";
    opendir(DIR,$sourceDir) || die "\n Can't open: $!\n";
    @pisaFileList = readdir(DIR);
}
else {
    print "\n\n Unable to locate top directory $topDirectory\n";
    print "    Exiting without further checking\n";
    exit;
} # safety check on existence of top directory

if($monthday<10) {
$gridCopy = $gridCopy . "0";
}
$gridCopy = "$topDirectory$gridCopy$monthday$monthYear{$month}$trueYear" . ".csh";
my $gridCopyLog = "$gridCopy" . ".log";
if(-e "$gridCopy") {
    `rm $gridCopy`;
}
open(GRIDCOPY, ">$gridCopy");
print GRIDCOPY "#! /bin/csh\n";
print GRIDCOPY "source /accre/vdt/setup.csh\n";

my $endName = "-0080.rootg";
my $startNumber = 80701;
my $endNumber = 83700;
my $nRuns = $endNumber - $startNumber + 1;
my @foundDSTRun;
my @foundCNTRun;
for(my $iRun=0; $iRun<$nRuns; $iRun++) {
    $foundDSTRun[$iRun] = 0;
    $foundCNTRun[$iRun] = 0;
}

my $fileCount = 0;
my $missingCNT = 0;
my $missingDST = 0;
my $fileName;

chdir "done";
my $nName = 0;
foreach $fileName (@pisaFileList) {
    $nName++;
    if($nName > 2) {
	if($fileName!~/DST/ && $fileName!~/CNT/) {
	    die "\n file name $fileName doesn't contain CNT or DST\n";
	}
	my @components = split(/-/, $fileName, 5);
	#die "\n $fileName with @components\n";
	my $testRun = $components[1];
	my $index = $testRun - $startNumber;
	if($index < 0 || $index >= $nRuns) {
	    die "\n invalid index number $index , for run number $testRun\n";
	}
	else {
	    if($fileName=~/DST/) {
		if($foundDSTRun[$index] == 1) {
		    die "\n DST index number $testRun already filled?\n";
		}
		$foundDSTRun[$index] = 1;
	    }
	    if($fileName=~/CNT/) {
		if($foundCNTRun[$index] == 1) {
		    die "\n CNT index number $testRun already filled?\n";
		}
		$foundCNTRun[$index] = 1;
	    }
	}
	$fileCount++;
	print GRIDCOPY "globus-url-copy -p 20 file://$sourceDir$fileName $destDir$fileName\n";
    }
}


for(my $iRun=0; $iRun<$nRuns; $iRun++) {
    if($foundDSTRun[$iRun] == 0) {
	$missingDST++;
    }
    if($foundCNTRun[$iRun] == 0) {
	$missingCNT++;
    }
}

print "\n Found files $fileCount ;  missing CNT files $missingCNT;  missing DST files $missingDST\n";

chdir $topDirectory;

close(GRIDCOPY);
`chmod +x $gridCopy`;

if($testMode != 1) {
  my $gridCopyLog = $gridCopy . ".log";
  `$gridCopy >& $gridCopyLog`;
}

 __END__
