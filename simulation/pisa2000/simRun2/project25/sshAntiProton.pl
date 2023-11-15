#!/usr/local/bin/perl
#
# Usage:  perl -w sshAntiProton.pl >& subAntiProton.out &
#
$topName = "/net/vpac12/rhic2/maguire/project25AntiProton/";  # top directory for this project's files
$pisaExe = "/home/maguire/bin/newPISA";     # location of PISA binary for this project

@nodes = ("vpac09", "drno", "vpac03", "vpac13", "vpac11", "vpac12",
          "vpac09", "drno", "vpac03", "vpac13", "vpac11", "vpac12",
          "vpac14", "vpac02", "vpac05", "vpac06", "vpac07", "thebrain",
          "vpac10", "drbig", "vpac06", "yakko", "vpac08");

#
# Leave numberStart and numberEnd as is, assuming sets of 20 jobs
#
$numberStart = 1;
$numberEnd = 20;
$numberPerCycle = 20;
$eventsPerRun = 55000;  # number of events per run

@cycleCount = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

$rndmBase0 = "rndm0";
$rndmBase = "rndm";
$cshPisaBase = "pisa";
$cshPisaExt = ".csh";
$outExt = ".out";

$oscProject = 25;   # project number
$oscVersion = 0;
$oscEvent = 27002;  # input event file ID number
$idBase = 27300;  # base PISA ID number for this event file
$iCycleStart = 0;
$nCycle = 5;      # assume 5 cycles total of 20 jobs

$oscNameFirst = "PISA2000_OSCAR01-00000";
$oscNameLast = "-0025.rootg";

$geantBase = 0;
$pisaInputBase = "pisaInput";

#
# Loop over rndm areas to compose input files and CSH job scripts
# First job script is submitted in this loop
#
for($number=$numberStart; $number<=$numberEnd; $number++) {

  if(-e $topName) {
    chdir $topName;
  }
  else {
    print "  Missing $topName \n";
    exit;
  }    

  $iNode = $number - 1;
  $idNumber = $idBase + $number;


  if($number<10) {
    $rndmxx = $rndmBase0 . $number;
  }
  if($number>9) {
    $rndmxx = $rndmBase . $number;
  }
   
  chdir $rndmxx;
  $dir = $topName . $rndmxx;

  if(-e "endit.dat") {
   `rm endit.dat`;
  }

  if(-e "lastCycle") {
    `rm lastCycle`;
  }

  if(-e "finishPisa") {
    `rm finishPisa`;
  }

  $idNumber = $idBase + $number;
  for($iCycle=$iCycleStart; $iCycle<$nCycle; $iCycle++) {

    $idGeant = $geantBase + $number + $iCycle*$numberPerCycle;
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
    $firstEvent = 1 + $eventsPerRun*($number + $iCycle*$numberPerCycle - 1);
    print FILE "oscar oscar.root $firstEvent \n";
    print FILE "ptrig $eventsPerRun\n";
    print FILE "exit\n";
    close(FILE);

    $cshScriptCycle = $cshPisaBase . $iCycleNumber . $cshPisaExt;

    if(-e $cshScriptCycle) {
     `rm $cshScriptCycle`;
    }

    $finishCycleNumber = "finish" . $iCycleNumber;
    if(-e $finishCycleNumber) {
      `rm $finishCycleNumber`;
    }

    if($iCycle==$iCycleStart) {
      $cshStartScript = $cshScriptCycle;
    }

    open(FILE, ">$cshScriptCycle");
    print FILE "#! /bin/csh -f\n";
    print FILE "cd $dir\n";
    print FILE "nice +14 $pisaExe  < $pisaInput >& pisa$iCycleNumber$outExt &\n";
    `chmod u+x $dir/$cshScriptCycle`;

  }

  print " Submitting script $dir/$cshStartScript \n";
  `ssh $nodes[$iNode] -n $dir/$cshStartScript`;

} # loop over numbers for first round of ssh submission

#
# Set up for second round of ssh submissions
# Look for finishCycle and lastCycle files in the rndmxx directories
#

print "\n\n  Checking for completed jobs\n\n";

$nLastCycle = 0;

while($nLastCycle<$numberPerCycle) {
  sleep 15;  # go dormant for 15 seconds
  #
  # Loop over rndm areas to check for completed jobs and resubmit CSH job scripts
  #
  for($number=$numberStart; $number<=$numberEnd; $number++) {

    $iNode = $number - 1;
    $idNumber = $idBase + $number;
    chdir $topName;
    if($number<10) {
      $rndmxx = $rndmBase0 . $number;
    }
    if($number>9) {
      $rndmxx = $rndmBase . $number;
    }
   
    chdir $rndmxx;
    $dir = $topName . $rndmxx;

    $idNumber = $idBase + $number;
    if(-e "finishPisa") {
      $iCycle = $cycleCount[$iNode];
      $iCycleNumber = $idNumber + $iCycle*$numberPerCycle;
      $completeCycleNumber = "complete" . $iCycleNumber;
      `mv finishPisa $completeCycleNumber`;
      `chmod u-w PISAEvent.root`;
      $newName = $oscNameFirst . $iCycleNumber . $oscNameLast;
      `mv PISAEvent.root $newName`;
      `mv $newName ../done`;

      $iCycle++;
      if($iCycle==$nCycle) {
        $nLastCycle++;
        print "Found a last cycle file in $rndmxx ; current nLastCycle = $nLastCycle\n";
      }  # found a finish cycle file

      if($iCycle<$nCycle) { 
        $cycleCount[$iNode]++; 
        $iCycleNext = $iCycleNumber + $numberPerCycle;
        $cshScriptNext = $cshPisaBase . $iCycleNext . $cshPisaExt;
         print "Submitting script $dir/$cshScriptNext in $rndmxx\n";
         `ssh $nodes[$iNode] -n $dir/$cshScriptNext`;

      }  # still cycling in this rndm area

    }  # check on finding a finishPisa file in this rndm area

  } # Second round of ssh submissions for loop over random areas

} # while loop waiting for all the rndm areas to show a lastCycle file

exit;
