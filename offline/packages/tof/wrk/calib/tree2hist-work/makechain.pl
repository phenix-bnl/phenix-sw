#!/usr/local/bin/perl
#-----------------------------------------------------------------------------
#  Perl script makechain.pl
#
#  Purpose: merge micro-dst
#
#  ARGV[0]: DST file name
#-----------------------------------------------------------------------------
if ($#ARGV > -1) {
    #
    # Anybody asked for help?
    #
    if ($ARGV[0] eq "-h" || $ARGV[0] eq "--help" || $ARGV[0] eq "-?") {
	print "\n";
	print "** generate the root macro for microDST analysis\n";
	print "-----------------------------------------------------------\n";
	print "arguments:                                                 \n";
	print "          makechain [classname] [listname]                 \n";
	print "   ARGV[0]: Class name \n";
	print "   ARGV[1]: microDST list \n";
	print "\n";
	exit;
    }
}
if ($#ARGV >= 0) {
    $class    = $ARGV[0];
    $filename = $ARGV[1];
#    $filename = $ARGV[0];
} else {
    print"usage makechain [-h] [--help] [-?] className \n";
    exit;
}
my $PWD="/bin/pwd";

print("  ",$nfs_host,"  ",$data_dir,"  ",$filename,"\n");
@filename0  = split(/\./,$filename);
$funcName   = @filename0[0];
$cshName    = @filename0[0].".csh";
$jobName    = @filename0[0].".C";
$logName    = @filename0[0].".log";
$mergeName  = @filename0[0].".root";

#$className = "TreeToHist";
$className = $class;

#$libName = "libMyAnalysis.so";
#$libName = "libCalibration.so";
$rundepName = "tofGlobalT_rundep.txt";

$thisDir  = `$PWD`;
chomp($thisDir); # remove \n character

$nLines = 0;                      # counter for output filename

open (FLIST, "$filename") || die "cannot open $filename \n";
@line = <FLIST>;

open (JOBFILE, ">$jobName") || die "cannot open ROOT MACRO file $jobName\n";
print JOBFILE "// merge.C\n";
print JOBFILE "//\n";
print JOBFILE "void $funcName(const char *ofname=\"$mergeName\"){\n";
#print JOBFILE "  gSystem->Load(\"$libName\");\n";
print JOBFILE "  gROOT->Macro(\"tofCalibByTreeInit.C\");\n";
print JOBFILE "  $className *ana = new $className(TofAddress, TofGeometry, TofCalib);\n";
print JOBFILE "  ana->setTofGlobalT(\"$rundepName\");\n";
print JOBFILE "\n";

foreach ( @line ){
    chomp;
    $inputArg=$_;
    print JOBFILE "  ana->Loop_a_file(\"$inputArg\");\n";
    #print JOBFILE "  cout<<\" add $ntpDir/$inputArg \"<<endl;\n";
    $nLines +=1;
}
print JOBFILE "\n";
print JOBFILE "  ana->Write(ofname);\n";
print JOBFILE "}\n";

open (CSHFILE, ">$cshName") || die "cannot open CSH file $cshName\n";
print CSHFILE "#!/bin/csh -f \n";
print CSHFILE "\n";
#print CSHFILE "setenv ROOTSYS /opt/phenix/root\n";
#print CSHFILE "source /opt/phenix/bin/phenix_setup.csh pro\n";
print CSHFILE "source phenix.csh\n";
print CSHFILE "\n";
print CSHFILE "# Post-DST Analysis\n";
print CSHFILE "root -b << EOF >& $logName\n"; # choice 1
print CSHFILE ".x $jobName;\n";
print CSHFILE ".q\n";
print CSHFILE "EOF\n";
print CSHFILE "\n";

system("chmod +x $cshName");

print("  Create script file ",$jobName,"   ",$nLines," lines\n");
