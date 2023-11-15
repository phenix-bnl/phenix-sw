#!/usr/local/bin/perl -w
# the line above this one should be the first line of the script
#
# Usage  (version January 10, 2007 at 11:14)
# perl condorReco.pl -nEvents=N
#   N is the number of events to be reconstructed in each PISA hits file (0 means all events)
#
# Output is a Condor submission script jobName taking as input a set of jobNameXXXX.csh scripts:
#             jobName is set by the user below, XXXX is over a range of numbers starting at 0
#

use strict;
use Getopt::Long;

#
# The name of the user's top directory for this project is given by the $topdir variable
#
my $topdir = "/phenix/data11/caidala/project89/";

my $jobName = "proj89PiPlus";   # name given to the condor submission script and the condor input .csh scripts
my $testMode = 0; # if testMode = 1 then there is no condor submission; if testMode = 0 then there is a condor submission (be careful here) 
my $jobSize = 500; # size of simulation reconstruction job in MBytes, you should test this for your own jobs

#
# It is assumed that there exists a dstEvents subdirectory in $topdir which will contain the produced nanoDSTs
# It is assumed that there exists a reco subdirectory in $topdir containing the pisaToDST.C and the pisaToDST_IOManager.C ROOT control macros
# It is assumed that there exist a condor subirectory in $topdir containing this condorReco.pl script generator
# It is assumed that there exists input and output subdirectories in the condor subdirectory
#
# A condor submission script jobNameCondor is created by this PERL script in the condor subdirectory
# The jobNameCondor script will be submitted automatically if the $testMode variable below is set to 0
# The condor .csh scripts are written by the condorReco.pl script into the input subdirectory ../condor/input
# The condor log, err, and out files are written by jobNameCondor into the output subdirectory ../condor/output
# You may want to remove previous versions of the log, err, and out files since the new versions will be appended to the old versions.
#
# You SHOULD test the condor submission script manually for a few jobs (e.g. change its last line to Queue 2) before changing $testMode to 0
# You don't want to flood the Condor system with useless job requests which will decrease your priority for getting future jobs done
#

#
# The PISA hits files are located in the $pisadir directory (could be a different user name than the one for $topdir)
#
my $pisadir =  "/phenix/data11/maguire/project89/pisaEvents/";

# start of the PISA hits file name (before sequence number)
my $pisaNameBegin = "PISA2000_PIPLPLPL-00000";

# end of the PISA hits file name (after sequence number)
my $pisaNameEnd = "-0089.rootg";

# range of sequence numbers for these PISA hit files
my $startNumber = 90431;
my $endNumber = 90432;

# Make sure to change the following two person names
my $userName = "caidala";  # name to use on the local /tmp directories of the rcas nodes
my $userEMail = "caidala\@bnl.gov";

#
# No changes normally needed in the code lines below
#
my $nEvents = -1;
GetOptions("nEvents=s" => \$nEvents);

if($nEvents<0) {
   die "Input parameter -nEvents is missing, or negative";
}

if($nEvents>=0) {
    if($nEvents>0) {
	print "\n  Number of events to be reconstructed for each input file = $nEvents\n";
    }
    if($nEvents==0) {
	print "\n All events will be reconstructed for each input file\n";
    }
}

my $dstdir = $topdir . "dstEvents/";
my $recodir = $topdir . "rcfReco/";
my $condordir = $topdir . "condor/";
my $condorInput = $condordir . "input/";
my $condorOutput = $condordir . "output/";

# check for the existence of all the needed subdirectories
if(!-e $topdir) {
    die "\n Top directory $topdir does not exist\n";
}
if(!-e $dstdir) {
    die "\n DST directory $dstdir does not exist\n";
}
if(!-e $recodir) {
    die "\n reco directory $recodir does not exist\n";
}
if(!-e $condorInput) {
    die "\n condor input directory $condorInput does not exist\n";
}
if(!-e $condorOutput) {
    die "\n condor output directory $condorOutput does not exist\n";
}

my $pisaToDSTC = $recodir . "pisaToDST.C";
if(!-e $pisaToDSTC) {
    die "\n file $pisaToDSTC does not exist\n";
}

my $pisaToDST_IOManagerC = $recodir . "pisaToDST_IOManager.C";
if(!-e $pisaToDST_IOManagerC) {
    die "\n file $pisaToDST_IOManagerC does not exist\n";
}

my $pisaToDSTInput = $recodir . "pisaToDST.input";

#
# make the pisaToDST.input file containing the requested number of input events
#
chdir $recodir;
open(FILE,">pisaToDST.input");
print FILE ".x pisaToDST.C($nEvents, \"simDst.root\", \"simMicroDST.root\", \"simNanoDST.root\", \"PISAEvent.root\", 3);\n";
print FILE ".q;\n";
close(FILE);

chdir $condorInput;
#
# loop over the range of PISA hits files
#
my $nMissing = 0;
my $nFound = 0;
my $iCondor = 0;
for (my $iRun=$startNumber; $iRun<=$endNumber; $iRun++) {
    my $pisaName = $pisadir . $pisaNameBegin. $iRun . $pisaNameEnd;
    if(-e  $pisaName) {
	$nFound++;
        #
        # At this point we could test for a minimum size of the PISA hits file
        # This test is not implemented at present
        #
	my $recoScriptName = "$jobName$iCondor" . ".csh";
	open(FILE, ">$recoScriptName");
	print FILE "#!/bin/csh -f\n";
	my $hostName = $dstdir . "rcfHost$iRun" . ".txt";
	print FILE "echo \$HOST > $hostName\n";
        print FILE "if(-e /tmp/$userName$iRun) then\n";
        print FILE "  rm -rf /tmp/$userName$iRun\n";
        print FILE "endif\n";
	print FILE "mkdir /tmp/$userName$iRun\n";
	print FILE "cd  /tmp/$userName$iRun\n";

        # 
        # default libraries (latest new version)
        # can also add line for additional LD_LIBRARY_PATH
        #
	print FILE "source /opt/phenix/bin/phenix_setup.csh\n";
	print FILE "setenv LD_LIBRARY_PATH /phenix/data12/caidala/install/lib:\$LD_LIBRARY_PATH\n";

	print FILE "ln -fs /afs/rhic/phenix/software/simulation/head/crk_cabling_vrdc.txt .\n";
     	print FILE "cp $pisaToDSTC .\n";
	print FILE "cp $pisaToDST_IOManagerC .\n";
	print FILE "cp $pisaToDSTInput .\n";
	print FILE "ln -fs $pisaName PISAEvent.root\n";
	print FILE "root -b < pisaToDST.input >& pisaToDST.log\n";
        my $nanoDstName = $dstdir . "simNanoDST$iRun" . ".root";
	print FILE "cp simNanoDST.root $nanoDstName\n";
	my $logName = $dstdir . "simNanoDST$iRun" . ".log";
	print FILE "cp pisaToDST.log $logName\n";
	print FILE "rm -rf /tmp/$userName$iRun\n";
	close(FILE);
        chmod 0755, $recoScriptName;
	$iCondor++;

    }
    else {
	$nMissing++;
	print "\n input file $pisaName does not exist\n";
    }
} # loop over run numbers

print "\nSummary\n Number of PISA files found = $nFound\n";
if($nMissing>0) {
    print " Number of PISA files missing = $nMissing\n";
}
else {
    print " There were no missing PISA files\n";
}

#
# Now we will compose the condor submission script in the condor subdirectory
#
chdir $condordir;
my $condorScriptName = "$jobName" . "Condor";
open(FILE, ">$condorScriptName");
print FILE "Universe     = vanilla\n";
print FILE "Notification = Error\n";
print FILE "\n";
print FILE "Initialdir   = $condordir\n";
print FILE "Executable   = /bin/csh\n";
print FILE "Arguments    = $condorInput$jobName\$(Process).csh\n";
print FILE "Log          = $condorOutput$jobName\$(Process).log\n";
print FILE "Output       = $condorOutput$jobName\$(Process).out\n";
print FILE "Error        = $condorOutput$jobName\$(Process).err\n";
print FILE "Notify_user  = $userEMail\n";
print FILE "\n";
print FILE "Requirements = CPU_Speed >=1\n";
print FILE "Rank         = CPU_Speed\n";
print FILE "\n";
print FILE "CPU_Experiment != \"lsst\"\n";
print FILE "CPU_Experiment != \"atlas\"\n";
print FILE "\n";
print FILE "Priority     = 20\n";
print FILE "GetEnv       = True\n";
print FILE "Image_Size   = $jobSize Meg\n";
print FILE "+Experiment  = \"PHENIX\"\n";
print FILE "+Job_Type    = \"cas\"\n";
print FILE "\n";
print FILE "Queue $iCondor\n";
close(FILE);

print "\n Number of Condor jobs would be $iCondor\n";
if($testMode == 0 && $iCondor>0) {
    my $condorScriptLog = "$condorScriptName" .".log";
    system("condor_submit $condorScriptName >& $condorScriptLog");
    print "\n The Condor script $condorScriptName has been submitted for reconstructing $iCondor PISA hits files\n\n";
}
else {
    print "\n The $condorScriptName Condor script was NOT submitted\n\n";
}
__END__
