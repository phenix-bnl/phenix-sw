#!/usr/local/bin/perl
#
# Usage:  perl -w qsubPisa.pl >& subKMinus.out &
#
$topName = "/mrbig/maguire/project25KMinus/";  # top directory for this project's files
$pisaExe = "/home/maguirc/project25/pisa";     # location of PISA binary for this project

#
# Leave numberStart and numberEnd as is, assuming sets of 25 jobs
#
$numberStart = 1;
$numberEnd = 25;
$numberPerCycle = 25;
$eventsPerRun = 55000;  # number of events per run

$rndmBase0 = "rndm0";
$rndmBase = "rndm";
$pbsPisaBase = "pisa";
$pbsPisaExt = ".pbs";
$outExt = ".out";

$oscProject = 25;   # project number
$oscVersion = 0;
$oscEvent = 27005;  # input event file ID number
$idBase = 27500;  # base PISA ID number for this event file
$iCycleStart = 0;
$nCycle = 4;      # assume 4 cycles total of 25 jobs

$oscNameFirst = "PISA2000_OSCAR01-00000";
$oscNameLast = "-0025.rootg";

$geantBase = 0;
$pisaInputBase = "pisaInput";

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

  if(-e "endit.dat") {
   `rm endit.dat`;
  }

  if(-e "lastCycle") {
    `rm lastCycle`;
  }

  $idNumber = $idBase + $number;
  for($iCycle=$iCycleStart; $iCycle<$nCycle; $iCycle++) {

    $idGeant = $geantBase + $number + $iCycle*25;
    $iCycleNumber = $idNumber + $iCycle*25;

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
    $firstEvent = 1 + $eventsPerRun*($number + $iCycle*25 - 1);
    print FILE "oscar oscar.root $firstEvent \n";
    print FILE "ptrig $eventsPerRun\n";
    print FILE "exit\n";
    close(FILE);

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
    print FILE "#! /bin/tcsh\n";
    $logOut = $iCycleNumber . $outExt;
    print FILE "#PBS -o $topName$rndmxx/log$logOut\n";
    print FILE "#PBS -j oe\n";
    print FILE "#PBS -l walltime=02:40:00\n";
    print FILE "#PBS -l cput=02:20:00\n";
    print FILE "#PBS -l nodes=1\n";
    print FILE "cd $topName$rndmxx\n";
    print FILE "cp $pisaInput pisa.input\n";
    print FILE "$pisaExe < pisa.input >& pisa$iCycleNumber$outExt\n";
    $newName = $oscNameFirst . $iCycleNumber . $oscNameLast;
    print FILE "chmod u-w PISAEvent.root\n";
    print FILE "mv PISAEvent.root $newName\n";
    print FILE "mv $newName ../done\n";
    print FILE "touch finish$iCycleNumber\n";

    if($iCycle==$nCycle-1) {
      print FILE "touch lastCycle\n";
    }

    close(FILE);

  }

  print " Submitting script $pbsStartScript \n";
  `qsub $pbsStartScript`;

} # loop over numbers for first round of PBS submission

#
# Set up for second round of PBS submissions
# Look for finishCycle and lastCycle files in the rndmxx directories
#
$nLastCycle = 0;
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
          print "Submitting script $pbsScriptNext in $rndmxx\n";
          `qsub $pbsScriptNext`; # resubmit next job in this rndm area
        }  # found a finish cycle cile

      }  # loop to look for finish cycle file

    } # check for last cycle completed file

  } # Second round of PBS submissions for loop over random areas

} # while loop waiting for all the rndm areas to show a lastCycle file

exit;
