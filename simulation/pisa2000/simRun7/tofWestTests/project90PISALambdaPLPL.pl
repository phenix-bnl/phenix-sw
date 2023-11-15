#!/usr/local/bin/perl

#
# Script to produce PISA hits files on ACCRE (version February 23, 2007)
# PISA jobs will execute in local /tmp area and copy PISAEvent.root files to a specified $pisaDone area
#
#
# Usage:  perl -w project81PISAetaPLPL.pl >& project81PISAetaPLPL.log & (change name of script as required)
#         You should check the output log file
#         Initially there are no warnings about unused variables, or other error messages.
#
#         You should see lines like the following which indicate a successful job submission
#         Submitting script pisa67401.pbs, try number 1
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
$topName = "/scratch/maguire/Lambda/pisaEvents/posField/";    # top directory for this project's files
$pisaDone = "/gpfs2/RUN7PRDF/$myName/Lambda/pisaEvents/"; # destination directory for PISAEvent.root files

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
$numberPerCycle = $numberEnd - $numberStart + 1;   # of course this should be greater than 0
if($numberPerCycle<1) {
    print "\n\n  Invalid numberPerCycle value  $numberPerCycle\n";
    exit;
}  # safety check

$eventsPerRun = 10000;  # number of events per run
$nCycle = 10; # 4 cycles with total of 100 jobs (100*50,000 = 5,000,000 events)

$oscProject = 90;   # project number
$oscVersion = 0;
$oscEvent = 90801;  # input event file ID number
$idBase = 90800;    # base PISA ID number for this event file

#
# Set up parts of input and output file names
#
$oscNameFirst = "PISA2000_LAMBDAPLPL-00000";  # name means eta in ++ field
$oscNameLast = "-00$oscProject.rootg";

#
# No need to change anything else below except the PBS wall clock and CPU times
#
$wallClockTime = "00:90:00"; # in hours:minutes:seconds, change as required
$cpuTime = "00:89:00";  # in hours:minutes:seconds, change as required and less than wallClockTime

if(-e $pisaDone) {
    print "\n PISAEvent files will be written to $pisaDone\n";
}
else {
    die "\n Destination directory $pisaDone for PISAEvent files does not exist";
}

#
# Leave the following variables as is
#
$iCycleStart = 0;   # leave as is
$geantBase = 0;
$pisaInputBase = "pisaInput";

$rndmBase0 = "rndm0";
$rndmBase = "rndm";
$pbsPisaBase = "pisa";
$pbsPisaExt = ".pbs";
$outExt = ".out";

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

  if(-e "endit.dat") {
   `rm endit.dat`;
  }

  if(-e "lastCycle") {
    `rm lastCycle`;
  }

  $idNumber = $idBase + $number;
  for($iCycle=$iCycleStart; $iCycle<$nCycle; $iCycle++) {

    $idGeant = $geantBase + $number + $iCycle*$numberPerCycle;
    $preCall = 0;
    if($idGeant > 200) {
      $preCall = int($idGeant/200);
    }
    $idGeant = $idGeant%200;  # allowed GEANT seeds from 1 to 215, stop at 200

    if($idGeant==0) {
      $idGeant = 200;
    }

    $iCycleNumber = $idNumber + $iCycle*$numberPerCycle;

    $pisaInput = $pisaInputBase . $iCycleNumber;
    if(-e $pisaInput) {
     `rm $pisaInput`;
    }

    open(FILE, ">$pisaInput");
    print FILE "0\n";
    print FILE "N\n";
    print FILE "0\n";
    print FILE "RNDM ", $idGeant," 000\n";
    print FILE "RUNN ", $oscEvent, " ", $iCycleNumber, " ", $oscProject, " ", $oscVersion,"\n";
    $firstEvent = 1 + $eventsPerRun*($iCycleNumber - $idBase - 1);
    print FILE "oscar oscar.root $firstEvent \n";
    print FILE "ptrig $eventsPerRun\n";
    print FILE "exit\n";
    close(FILE);

    $pbsScriptCycle = $pbsPisaBase . $iCycleNumber . $pbsPisaExt;
    $myNameCycleNumber = $myName . $iCycleNumber;

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
    print FILE "#! /bin/tcsh\n";
    $logOut = $iCycleNumber . $outExt;
    print FILE "#PBS -o $topName$rndmxx/log$logOut\n";
    print FILE "#PBS -j oe\n";
    print FILE "#PBS -l walltime=$wallClockTime\n";
    print FILE "#PBS -l cput=$cpuTime\n";
    print FILE "#PBS -l nodes=1:ppn=1:x86\n";
    print FILE "#PBS -l mem=450mb\n";
    print FILE "echo \$HOST\n";
    print FILE "cd $topName$rndmxx\n";
    print FILE "echo \$HOST > accreHost$iCycleNumber\n";
    print FILE "source /home/phenix/phnxVandy_setup.csh\n";
    print FILE "cp $pisaInput pisa.input\n";
    print FILE "date\n";
    print FILE "ls -al /tmp\n";
    print FILE "rm -rf /tmp/$myNameCycleNumber\n";  # safety delete if left over from previous job
    print FILE "mkdir /tmp/$myNameCycleNumber\n";
    print FILE "cp pisa.input /tmp/$myNameCycleNumber\n";
    print FILE "cp event.par /tmp/$myNameCycleNumber\n";
    print FILE "cp flukaaf.dat /tmp/$myNameCycleNumber\n";
    print FILE "cp gffgo.dat /tmp/$myNameCycleNumber\n";
    print FILE "cp glogon.kumac /tmp/$myNameCycleNumber\n";
    print FILE "cp oscar.root /tmp/$myNameCycleNumber\n";
    print FILE "cp phnx.par /tmp/$myNameCycleNumber\n";
    print FILE "cp pisa.kumac /tmp/$myNameCycleNumber\n";
    print FILE "cp Sim3D++.root /tmp/$myNameCycleNumber\n";
    print FILE "cp xsneut95.dat /tmp/$myNameCycleNumber\n";
    print FILE "date\n";
    print FILE "cd /tmp/$myNameCycleNumber\n";
    print FILE "df -k /tmp\n";
    print FILE "pisa < pisa.input >& pisa$iCycleNumber$outExt\n";
    $newName = $oscNameFirst . $iCycleNumber . $oscNameLast;
    print FILE "mv PISAEvent.root $newName\n";
    print FILE "cp $newName $pisaDone\n";
    print FILE "chmod u-w $pisaDone$newName\n";
    print FILE "cd $topName$rndmxx\n";
    print FILE "cp /tmp/$myNameCycleNumber/pisa$iCycleNumber$outExt .\n";
    print FILE "touch finish$iCycleNumber\n";

    if($iCycle==$nCycle-1) {
      print FILE "touch lastCycle\n";
    }

    print FILE "rm -rf /tmp/$myNameCycleNumber\n";
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
              chomp($status); # delete any carriage return
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

	  sleep 15;  # go dormant for 15 seconds before checking next rndm area

        }  # found a finish cycle file

      }  # loop to look for finish cycle file

    } # check for last cycle completed file

  } # Second round of PBS submissions for loop over random areas

} # while loop waiting for all the rndm areas to show a lastCycle file

print "\n\n All cycles completed\n\n";

exit;
