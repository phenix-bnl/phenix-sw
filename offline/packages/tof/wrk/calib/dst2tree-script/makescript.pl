#!/usr/local/bin/perl
#-----------------------------------------------------------------------------
#  Perl script makescript.pl
#
#  Purpose: loop maketree.pl
#
#  ARGV[0]: DST file name
#-----------------------------------------------------------------------------
if ($#ARGV > -1) {
    #
    # Anybody asked for help?
    #
    if ($ARGV[0] eq "-h" || $ARGV[0] eq "--help" || $ARGV[0] eq "-?") {
	print "\n";
	print "*** makentuple, generate the csh script for dstana_v2.C ***\n";
	print "-----------------------------------------------------------\n";
	print "arguments:                                                 \n";
	print "          makescript [data_dir] [listname]                 \n";
	print "   ARGV[0]: Directory \n";
	print "   ARGV[1]: DST file list \n";
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

print("  DST: ",$data_dir,"\n");
print(" LIST: ",$filename,"\n");
@filename0  = split(/\./,$filename);
$jobName    = @filename0[0].".csh";

$nLines = 1;                      # counter for output filename

open (FLIST, "$filename") || die "cannot open $filename \n";
@line = <FLIST>;

open (JOBFILE, ">$jobName") || die "cannot open CSH file $jobName\n";
print JOBFILE "#!/bin/csh -f \n";
print JOBFILE "\n";

foreach ( @line ){
    chomp;
    $inputArg=$_;

    #print($inputArg, " is ", $nLines," th line","\n");
    system("maketree.pl $data_dir $inputArg");

    @name     = split(/\./,$inputArg);
    @section  = split(/\-/,@name[0]);
    $runnum   = @section[1];
    $seqnum   = @section[2];
    $cshDir   = "csh/";
    $cshName  = join("", "maketree".$runnum,"_",$seqnum,".csh");

    $logDir   = "log/";
    $outputName   = "job".$runnum."_".$seqnum.".output";
    $errorName    = "job".$runnum."_".$seqnum.".error";

    $QUEUE = "sim_short";  ## CCJ
    #$QUEUE = "sim_long";  ## CCJ
    #$QUEUE = "phenix_cas";  ## RCF
    print JOBFILE "bsub -q $QUEUE -o $logDir$outputName -e $logDir$errorName ./$cshDir$cshName\n";

    print JOBFILE "sleep 5\n";

    use integer;
    $i = $nLines%30;
    if($i == 0 ){
	print JOBFILE "sleep 300\n";
    }
    $nLines +=1;
}

print("\n  Create script file ",$jobName,"   ",$nLines," lines\n");
system("chmod +x $jobName");


