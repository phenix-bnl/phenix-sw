#!/usr/local/bin/perl

# Script to produce nanoDSTs from PISA hits files on ACCRE (version May 20, 2006)
#
# Usage:  perl -w project90ReadLambdaPLPL.pl >& project90ReadLambdaPLPL.log & (change name of script as required)
#         You should check the output log file
#         Initially there are no warnings about unused variables, or other error messages.
#
#         You should see lines like the following which indicate a successful job submission
#         Submitting script pisaRead63701.pbs, try number 1
#          Status from qsub 232404.vmpsched  with length 15
#
#         When all cycles are completed you should see lines like the following
#            Found a lastCycle file in rndm50 ; current nLastCycle = 50
#
#            All cycles completed
#

#
# Change the following name for the top directory of this project
#
$myName = "maguire";
$topName = "/scratch/$myName/Lambda/pisaEvents/posField/";  # top directory for this project's files
if(-e $topName) {
    chdir $topName;
}
else {
    print "\n\n Unable to locate top directory $topName\n";
    print "    Exiting without further checking or submitting any jobs\n";
    exit;
} # safety check on existence of top directory

#
# the checkMode variable is used to test the job submission script without actually using the qsub command
# if checkMode = 1, then the rndm areas are all filled with the PBS and input scripts
#                   these rndm areas can be checked to see that PBS and input scripts are correct
# if checkMode = 0, then the job scripts are actually submitted with the qsub command
#
$checkMode = 1;   # default to be in check mode with no PBS submissions

#
# Leave numberStart and numberEnd as is, assuming sets of 25 jobs
#
$numberStart = 1;
$numberEnd = 50;
$numberPerCycle = $numberEnd - $numberStart + 1;  # of course this should be greater than 0
if($numberPerCycle<1) {
    print "\n\n  Invalid numberPerCycle value  $numberPerCycle\n";
    exit;
}  # safety check

$nCycle = 10; # 1 cycles totaling 500 jobs (500*10000 = 5,00,000 events)

$oscProject = 90;   # project number (change as required for this project)
$idBase = 90800;    # base PISA ID number for this event file  (change as required for this project)

#
# Set up parts of input and output file names
#
$pisaDirectory = "/scratch/maguire/Lambda/pisaEvents/posField/done/"; # (change as required)
$padNameFirst = "ancpad-";     # (change as required for the ROOT NTUPLE)
$padName2First = "ancpad2-";     # (change as required for the ROOT NTUPLE)
$tfwNameFirst = "anctfw-";     # (change as required for the ROOT NTUPLE)
$tfwName2First = "anctfw2-";     # (change as required for the ROOT NTUPLE)
$oscNameFirst = "PISA2000_LAMBDAPLPL-00000";    # (change as required for the PISA hits file name)
$oscNameLast = "-00$oscProject.rootg";            # (change as required for the PISA hits file name)
$pisaReadInput = "pisaRead.input";
$pisaReadMacro = "pisaRead.C";

#
# write the host name in a text file
#
`echo \$HOST  > $topName/readLambdaPosField.log`;

#
# No need to change anything else below except the PBS wall clock and CPU times
#
$wallClockTime = "0:20:00"; # in hours:minutes:seconds, change as required
$cpuTime = "0:19:30";  # in hours:minutes:seconds, change as required and less than wallClockTime

#
# Leave the following variables as is
#
$iCycleStart = 0;   # leave as is
$rndmBase0 = "rndm0";
$rndmBase = "rndm";
$pbsPisaBase = "pisaRead";
$pbsPisaExt = ".pbs";
$outExt = ".out";

$copyPISA = "copyPISA/";
$copyPISAFlag = "copyPISAFlag";
$startCopy = "startCopy";
$endCopy = "endCopy";

if(-e "$topName$copyPISA") {
   `rm -rf $topName$copyPISA`;
}
mkdir "$topName$copyPISA";

#
# Loop over rndm areas to compose input files and PBS job scripts
# First job script is submitted in this loop
#
for($number=$numberStart; $number<=$numberEnd; $number++) {
  $idNumber = $idBase + $number;

  chdir $topName;

  if($number<10) {
    $rndmxx = $rndmBase0 . $number;
  }
  if($number>9) {
    $rndmxx = $rndmBase . $number;
  }
   
  chdir $rndmxx;

  if(-e "lastCycle") {
    `rm lastCycle`;
  }

  $idNumber = $idBase + $number;

  for($iCycle=$iCycleStart; $iCycle<$nCycle; $iCycle++) {

    $iCycleNumber = $idNumber + $iCycle*$numberPerCycle;

    $pbsScriptCycle = $pbsPisaBase . $iCycleNumber . $pbsPisaExt;

    if(-e $pbsScriptCycle) {
     `rm $pbsScriptCycle`;
    }

    $finishCycleNumber = "finish" . $iCycleNumber;
    if(-e $finishCycleNumber) {
      `rm $finishCycleNumber`;
    }

    if($iCycle==$iCycleStart) {
      $pbsStartScript = $pbsScriptCycle;
    }

    open(FILE, ">$pbsScriptCycle");
    print FILE "#! /bin/tcsh -f\n";
    $logOut = $iCycleNumber . $outExt;
    print FILE "#PBS -o $topName$rndmxx/logRead$logOut\n";
    print FILE "#PBS -j oe\n";
    print FILE "#PBS -l walltime=$wallClockTime\n";
    print FILE "#PBS -l cput=$cpuTime\n";
    print FILE "#PBS -l nodes=1:ppn=1:x86\n";
    print FILE "#PBS -l mem=450mb\n";
    print FILE "setenv LD_LIBRARY_PATH /home/maguirc1/intelInstall/lib:\$LD_LIBRARY_PATH\n";  # using personal library area since February 16 pisaRootRead not up to date
    print FILE "setenv PATH /home/maguirc1/intelInstall/bin:\$PATH\n";  # using personal bin area since February 16 pisaRootRead not up to date
    print FILE "cd $topName$rndmxx\n";
    print FILE "date\n";
    print FILE "echo \$HOST\n";
    print FILE "echo \$HOST > accreHostRead$iCycleNumber\n";
    $pisaInputName = $pisaDirectory . $oscNameFirst . $iCycleNumber . $oscNameLast;
    print FILE "df -k /tmp\n";
    print FILE "ls -al /tmp\n";
    print FILE "rm -rf /tmp/$myName$iCycleNumber\n";  # safety delete
    print FILE "mkdir /tmp/$myName$iCycleNumber\n";
    print FILE "# \n";
    print FILE "# Logic to allow only one PISAEvent.root file copy at a time\n";
    print FILE "# \n";
    print FILE "\@ notdone = 1\n";
    print FILE "\@ counter = 0\n";
    $sleepTime = 10 + rand(60);
    $sleepTime = int($sleepTime);
    $countSleep = 3600/$sleepTime;
    $countSleep = int($countSleep);
    print FILE "while ( \$notdone == 1 )\n";
    print FILE "  sleep $sleepTime\n";
    print FILE "  if( \$counter >= $countSleep) then\n";
    print FILE "    break\n";
    print FILE "  endif\n";
    print FILE "  if( -e \"$topName$copyPISA$copyPISAFlag\") then\n";
    print FILE "    \@ notdone = 1\n";
    print FILE "  else\n";
    print FILE "    touch $topName$copyPISA$copyPISAFlag\n";
    print FILE "    \@ notdone = 0\n";
    print FILE "    break;\n";
    print FILE "  endif\n";
    print FILE "  @ counter++\n";
    print FILE "end\n";
    print FILE "echo \" \"\n";
    print FILE "echo \"Wait counter is \$counter\"\n";
    print FILE "echo \" \"\n";
    print FILE "touch $topName$copyPISA$startCopy$iCycleNumber\n";
    print FILE "cp $pisaInputName /tmp/$myName$iCycleNumber/PISAEvent.root\n";
    print FILE "cp $topName$pisaReadMacro /tmp/$myName$iCycleNumber\n";
    print FILE "cp $topName$pisaReadInput /tmp/$myName$iCycleNumber\n"; 
    print FILE "touch $topName$copyPISA$endCopy$iCycleNumber\n";
    print FILE "rm $topName$copyPISA$copyPISAFlag\n";
    print FILE "cd /tmp/$myName$iCycleNumber\n";
    print FILE "ls -al\n";
    print FILE "date\n";
    print FILE "root -b < pisaRead.input >& pisaRead$iCycleNumber$outExt\n";
    print FILE "date\n";
    print FILE "cd $topName$rndmxx\n";
    print FILE "cp /tmp/$myName$iCycleNumber/pisaRead$iCycleNumber$outExt .\n";
    $padName = $padNameFirst . $iCycleNumber . $oscNameLast;
    $padName2 = $padName2First . $iCycleNumber . $oscNameLast;
    $tfwName = $tfwNameFirst . $iCycleNumber . $oscNameLast;
    $tfwName2 = $tfwName2First . $iCycleNumber . $oscNameLast;
    print FILE "cp /tmp/$myName$iCycleNumber/ancpad.root $padName\n";
    print FILE "cp /tmp/$myName$iCycleNumber/ancpad2.root $padName2\n";
    print FILE "cp /tmp/$myName$iCycleNumber/anctfw.root $tfwName\n";
    print FILE "cp /tmp/$myName$iCycleNumber/anctfw2.root $tfwName2\n";
    print FILE "mv $padName ../done\n";
    print FILE "mv $padName2 ../done\n";
    print FILE "mv $tfwName ../done\n";
    print FILE "mv $tfwName2 ../done\n";
    print FILE "touch finish$iCycleNumber\n";
    if($iCycle==$nCycle-1) {
      print FILE "touch lastCycle\n";
    }
    
    print FILE "rm -rf /tmp/$myName$iCycleNumber\n";
    print FILE "ls -al /tmp\n";
    print FILE "date\n";
    close(FILE);

  }

#
# Submitting job script
# The qsub command for PBS on ACCRE sometimes fails to work
# The following loop was added to try 5 times the submission for a given job
# After each qsub submission attempt the status of the PBS jobs queue is checked with the return code from qsub
# If the submitted job is found, then the loop exits
#
  for($iTry=1; $iTry<6; $iTry++) {
    print " Submitting script $pbsStartScript, try number $iTry\n";
    if($checkMode == 1) {
	last;  # break out of loop if in check mode
    }
    $status = `qsub $pbsStartScript`;
    chomp($status); # delete any carriage return
    $lenStatus = length($status);
    print "  Status from qsub $status  with length $lenStatus\n";
    if(length($status) == 0) {
	print "  qsub failed\n";
    }
    else {
	last;   # break out of loop after success return code
    }
    sleep 15;  # go dormant for 15 seconds
  }  # try 5 times for submission

} # loop over numbers for first round of PBS submission

if($checkMode == 1) {
    print "\n\n   Exiting from check mode, no jobs were actually submitted to PBS\n";
    exit;
} 

#
# Set up for second round of PBS submissions
# Look for finishCycle and lastCycle files in the rndmxx directories
#

$nLastCycle = 0;

print "\n\n Checking for completed jobs\n\n";

while($nLastCycle<$numberPerCycle) {
  sleep 5;  # go dormant for 5 seconds
  #
  # Loop over rndm areas to check for completed jobs and resubmit PBS job scripts
  #
  for($number=$numberStart; $number<=$numberEnd; $number++) {
    $idNumber = $idBase + $number;
    chdir $topName;
    if($number<10) {
      $rndmxx = $rndmBase0 . $number;
    }
    if($number>9) {
      $rndmxx = $rndmBase . $number;
    }
   
    chdir $rndmxx;

    if(-e "lastCycle") {
      $nLastCycle++;
      print "Found a lastCycle file in $rndmxx ; current nLastCycle = $nLastCycle\n";
      `mv lastCycle foundLastCycle`;
    }
    else {
  
      $idNumber = $idBase + $number;
      for($iCycle=$iCycleStart; $iCycle<$nCycle-1; $iCycle++) {

        $iCycleNumber = $idNumber + $iCycle*$numberPerCycle;
        $finishCycleNumber = "finish" . $iCycleNumber;
        $completeCycleNumber = "complete" . $iCycleNumber;
        if(-e $finishCycleNumber) {
          `mv $finishCycleNumber $completeCycleNumber`;
          $iCycleNext = $iCycleNumber + $numberPerCycle;
          $pbsScriptNext = $pbsPisaBase . $iCycleNext . $pbsPisaExt;
#
#   Same safety check on success of qsub command
#
	  for($iTry=1; $iTry<6; $iTry++) {
	      print " Submitting script $pbsScriptNext in $rndmxx, try number $iTry\n";
	      $status = `qsub $pbsScriptNext`;
              chomp($status); # remove any carriage return
	      $lenStatus = length($status);
	      print "  Status from qsub $status  with length $lenStatus\n";
	      if(length($status) == 0) {
		  print "  qsub failed\n";
	      }
	      else {
		  last;   # break out of loop after success return code
	      }
	      sleep 5;  # go dormant for 5 seconds before next try
	  }  # try 5 times for successful submission

	  sleep 5;  # go dormant for 5 seconds before checking next rndm area

        }  # found a finish cycle file

      }  # loop to look for finish cycle file

    } # check for last cycle completed file

  } # Second round of PBS submissions for loop over random areas

} # while loop waiting for all the rndm areas to show a lastCycle file

print "\n\n All cycles completed\n\n";

exit;
