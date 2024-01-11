#!/usr/local/bin/perl
#-----------------------------------------------------------------------------
#  Perl script maketree.pl
#
#  Purpose: auto-generate csh script for camDataRun.C
#
#  ARGV[0]: DST file name
#-----------------------------------------------------------------------------
if ($#ARGV > -1) {
    #
    # Anybody asked for help?
    #
    if ($ARGV[0] eq "-h" || $ARGV[0] eq "--help" || $ARGV[0] eq "-?") {
	print "\n";
	print "** maketree, generate the csh script for tofCalibByDST.C **\n";
	print "-----------------------------------------------------------\n";
	print "arguments:                                                 \n";
	print "          maketree [data_dir] [filename]                   \n";
	print "   ARGV[0]: Directory \n";
	print "   ARGV[1]: DST file name \n";
	print "\n";
	exit;
    }
}
if ($#ARGV >= 1) {
    $data_dir = $ARGV[0];
    $filename = $ARGV[1];
} else {
    print"usage makereco [-h] [--help] [-?] className \n";
    exit;
}
my $PWD="/bin/pwd";
#
# Write csh file
#
@name     = split(/\./,$filename);
@section  = split(/\-/,@name[0]);

$runnum   = @section[1];
$seqnum   = @section[2];

$event    = "1600";

$dstDir   = $data_dir;
$thisDir  = `$PWD`;
chomp($thisDir); # remove \n character


$treeDir  = $thisDir."/tree";
$logDir   = $thisDir."/log";
$cshDir   = $thisDir."/csh";
$wrkDir   = $thisDir."/wrk";

$treeName = "run".$runnum."tree".$seqnum.".root";
$logName  = "tree".$runnum."_".$seqnum.".log";
$cshName  = join("", "maketree".$runnum,"_",$seqnum,".csh");

$dstHost  = "ccjnfs1";  ## CCJ
$wrkHost  = "ccjnfs1";  ## CCJ

$jobDir   = "/job_tmp/kiyo".$runnum."_".$seqnum;   ## CCJ
#$jobDir   = $thisDir."/job_tmp/kiyo".$runnum."_".$seqnum;  ## RCF

open (CSHFILE, ">$cshDir/$cshName") || die "cannot open CSH file $cshName\n";
print CSHFILE "#!/bin/csh -f \n";
print CSHFILE "\n";
print CSHFILE "source phenix.csh\n";
print CSHFILE "mkdir $jobDir\n";
print CSHFILE "cp $wrkDir/tofCalibByDST.C $jobDir\n";
print CSHFILE "cp $wrkDir/tof*.txt $jobDir\n";
print CSHFILE "cp $wrkDir/tof*.txt.pass? $jobDir\n";
print CSHFILE "rcpx $dstHost:$dstDir/$filename $jobDir/\n";  ## CCJ
#print CSHFILE "ln -s $dstDir/$filename $jobDir/\n";  ## RCF
print CSHFILE "cd $jobDir/\n";
print CSHFILE "touch core\n";
print CSHFILE "chmod 444 core\n";
print CSHFILE "\n";
print CSHFILE "# Post-DST Analysis\n";
print CSHFILE "root -b << EOF >& $logName\n";
#print CSHFILE "#include <stddef.h>;\n";  ## for ROOT3.XX
print CSHFILE ".x tofCalibByDST.C($event, \"$filename\" ,\"$treeName\");\n";
print CSHFILE ".q\n";
print CSHFILE "EOF\n";
print CSHFILE "\n";
print CSHFILE "cd $thisDir\n";
print CSHFILE "rcpx $jobDir/$treeName $wrkHost:$treeDir/ \n";  ## CCJ
print CSHFILE "rcpx $jobDir/$logName $wrkHost:$logDir/ \n";    ## CCJ
#print CSHFILE "mv $jobDir/$treeName $treeDir/ \n";  ## RCF
#print CSHFILE "mv $jobDir/$logName $logDir/ \n";    ## RCF
print CSHFILE "\n";
print CSHFILE "rm -fr $jobDir\n";

system("chmod +x $cshDir/$cshName");
