#!/usr/local/bin/perl

# Script to produce nanoDSTs from PISA hits files on ACCRE (version May 20, 2006)
#
# Usage:  perl -w project83DSTOmega3PiMIMI.pl >& projectDSTOmega3PiMIMI.log & (change name of script as required)
#         You should check the output log file
#         Initially there are no warnings about unused variables, or other error messages.
#
#         You should see lines like the following which indicate a successful job submission
#         Submitting script pisaToDST63701.pbs, try number 1
#          Status from qsub 232404.vmpsched  with length 15
#
#         When all cycles are completed you should see lines like the following
#            Found a lastCycle file in rndm25 ; current nLastCycle = 25
#
#            All cycles completed
#

#
# Change the following name for the top directory of this project
#
$myName = "maguire";
$topName = "/scratch/$myName/0/project80/dstEvents/";  # top directory for this project's files

#
# the checkMode variable is used to test the job submission script without actually using the qsub command
# if checkMode = 1, then the rndm areas are all filled with the PBS and input scripts
#                   these rndm areas can be checked to see that PBS and input scripts are correct
# if checkMode = 0, then the job scripts are actually submitted with the qsub command
#
$checkMode = 0;   # default to be in check mode with no PBS submissions

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

$eventsPerRun = 1000;  # number of events per run (change as required)
$nCycle = 60; # 60 cycles totaling 3000 jobs (3000*1,000 = 3,000,000 events)

$oscProject = 80;   # project number (change as required for this project)
$idBase = 80700;    # base PISA ID number for this event file  (change as required for this project)

#
# Set up parts of input and output file names
#
$pisaDirectory = "/scratch/maguire/0/project80/pisaEvents/done/"; # (change as required)
$cntNameFirst = "CNT2000_Omega3PiMIMI-00000";     # (change as required for the CNT nanoDST)
$hwgNameFirst = "HWG2000_Omega3PiMIMI-00000";     # (change as required for the HWG nanoDST)
$dstNameFirst = "DST2000_Omega3PiMIMI-00000";     # (change as required for the DST)
$oscNameFirst = "PISA2000_Omega3PiMIMI-00000";    # (change as required for the PISA hits file name)
$oscNameLast = "-00$oscProject.rootg";            # (change as required for the PISA hits file name)

#
# write the host name in a text file
#
`echo \$HOST  > $topName/dstHostOmega3PiMIMI.log`;

#
# No need to change anything else below except the PBS wall clock and CPU times
#
$wallClockTime = "1:00:00"; # in hours:minutes:seconds, change as required
$cpuTime = "0:50:30";  # in hours:minutes:seconds, change as required and less than wallClockTime

#
# Leave the following variables as is
#
$iCycleStart = 0;   # leave as is
$rndmBase0 = "rndm0";
$rndmBase = "rndm";
$pbsPisaBase = "pisaToDST";
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

  if(-e $topName) {
      chdir $topName;
  }
  else {
      print "\n\n Unable to locate top directory $topName\n";
      print "    Exiting without further checking or submitting any jobs\n";
      exit;
  } # safety check on existence of top directory

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

  if(-e "pisaToDST.input") {
    `rm pisaToDST.input`;
  }

  open(FILE, ">pisaToDST.input");
  print FILE ".x pisaToDST.C($eventsPerRun)\n";
  print FILE ".q;\n";
  close(FILE);

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
    print FILE "#PBS -o $topName$rndmxx/log$logOut\n";
    print FILE "#PBS -j oe\n";
    print FILE "#PBS -l walltime=$wallClockTime\n";
    print FILE "#PBS -l cput=$cpuTime\n";
    print FILE "#PBS -l nodes=1:ppn=1:x86\n";
    print FILE "#PBS -l mem=450mb\n";
    print FILE "cd $topName$rndmxx\n";
    print FILE "date\n";
    print FILE "echo \$HOST\n";
    print FILE "echo \$HOST > accreHost$iCycleNumber\n";
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
    print FILE "cp AlwaysDeadCh_pp200_168676.dat /tmp/$myName$iCycleNumber\n";
    print FILE "cp crk_cabling_vrdc.txt /tmp/$myName$iCycleNumber\n";
    print FILE "cp DchEfficiency_pp200.Real /tmp/$myName$iCycleNumber\n";
    print FILE "cp DchGeometry.frame00NoRetracted /tmp/$myName$iCycleNumber\n";
    print FILE "cp DchGeometry.info /tmp/$myName$iCycleNumber\n";
    print FILE "cp DchGeometry.wireMc /tmp/$myName$iCycleNumber\n";
    print FILE "cp fieldIntegral.dat /tmp/$myName$iCycleNumber\n";
    print FILE "cp pisaToDST.C /tmp/$myName$iCycleNumber\n";
    print FILE "cp pisaToDST_IOManager.C /tmp/$myName$iCycleNumber\n";
    print FILE "cp pisaToDST.input /tmp/$myName$iCycleNumber\n"; 
    print FILE "touch $topName$copyPISA$endCopy$iCycleNumber\n";
    print FILE "rm $topName$copyPISA$copyPISAFlag\n";
    print FILE "cd /tmp/$myName$iCycleNumber\n";
    print FILE "ls -al\n";
    print FILE "date\n";
    print FILE "root -b < pisaToDST.input >& pisaToDST$iCycleNumber$outExt\n";
    print FILE "date\n";
    print FILE "cd $topName$rndmxx\n";
    print FILE "cp /tmp/$myName$iCycleNumber/pisaToDST$iCycleNumber$outExt .\n";
    print FILE "cp /tmp/$myName$iCycleNumber/simDST.root .\n";
    print FILE "cp /tmp/$myName$iCycleNumber/simCNT.root .\n";
    print FILE "cp /tmp/$myName$iCycleNumber/simHWG.root .\n";
    $cntDSTName = $cntNameFirst . $iCycleNumber . $oscNameLast;
    print FILE "chmod u-w simCNT.root\n";
    print FILE "mv simCNT.root $cntDSTName\n";
    print FILE "mv $cntDSTName ../done\n";
    $hwgDSTName = $hwgNameFirst . $iCycleNumber . $oscNameLast;
    print FILE "chmod u-w simHWG.root\n";
    print FILE "mv simHWG.root $hwgDSTName\n";
    print FILE "mv $hwgDSTName ../done\n";
    $dstDSTName = $dstNameFirst . $iCycleNumber . $oscNameLast;
    print FILE "chmod u-w simDST.root\n";
    print FILE "mv simDST.root $dstDSTName\n";
    print FILE "mv $dstDSTName ../done\n";
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

@currentCycle = ();
for($number=$numberStart; $number<=$numberEnd; $number++) {
  $currentCycle[$number] = 0;
}
$nCycleMinus1 = $nCycle - 1;

$nLastCycle = 0;

print "\n\n Checking for completed jobs\n\n";

while($nLastCycle<$numberPerCycle) {
  sleep 15;  # go dormant for 15 seconds
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
      $iCycle = $currentCycle[$number];
      #for($iCycle=$iCycleStart; $iCycle<$nCycle-1; $iCycle++) {
      if($iCycle < $nCycleMinus1) {

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
	      sleep 15;  # go dormant for 15 seconds before next try
	  }  # try 5 times for successful submission

          $currentCycle[$number] += 1;

        }  # found a finish cycle file

      }  # loop to look for finish cycle file

    } # check for last cycle completed file

  } # Second round of PBS submissions for loop over random areas

} # while loop waiting for all the rndm areas to show a lastCycle file

print "\n\n All cycles completed\n\n";

exit;
