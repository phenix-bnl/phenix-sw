eval 'exec perl -w -S $0 ${1+"$@"}'  # -*- Perl -*-
       if 0; 

use Env;
use Getopt::Long;

#
# Description: 
# Submit pisa jobs (runpisa.sh) 

#
# Set defaults
#
$cpulimit = "20:00";
$setname  = "project5";
$setnum   = "5";
$subset   = "protonwest";
$nosubmit = 0;
$nohpss   = 1;
$nruns    = 50000;
$nfirst   = 401;
$nlast    = 401;
$iseed    = 1;
$npercpu  = 20;

#
# Directory shortcuts (site specific)
#
$evtdir   = "/nfs/tmp2/$USER/simRun2/" . $setname . "/oscar";
$pisatmp  = "/nfs/tmp2/$USER/simRun2/" . $setname . "/pisatmp/" . $subset;
$pisawrk  = "/usr/gapps/phenix/cvs/simulation/pisa2000/wrk";
$projdir  = "/usr/gapps/phenix/cvs/simulation/pisa2000/simRun2/" . $setname;
$runpisa  = "$projdir" . "/runpisa.csh";

GetOptions(
	    "help"       => \$help,
            "nfirst=i"   => \$nfirst,
            "nlast=i"    => \$nlast,
            "nruns=i"    => \$nruns,
	    "iseed=i"    => \$iseed,
	    "nosubmit"   => \$nosubmit,
	    "nohpss"     => \$nohpss,
	    "cpulimit=s" => \$cpulimit,
	    "setname=s"  => \$setname,
	    "subset=s"   => \$subset
	   );

if ($help) {
    print(
	  "Usage: $0 [options] \n",
	  "Options:\n",
	  "    --help         Print help message.\n",
	  "    --nfirst       First job (file number).\n",
	  "    --nlast        Last job (file number).\n",
	  "    --nruns        Number of events per file.\n",
	  "    --iseed        Grndm starting cycle.\n",
	  "    --nosubmit     Do not submit jobs.\n",
	  "    --nohpss       Do not copy files off to storage.\n",
          "    --cpulimit=<s> cpu limit string provided to psub (-tm).\n",
          "    --setname=<s>  Use set different set name.\n",
          "    --subset=<s>   Set the subset letter.\n"
	  );
    exit;
}
#
# Fixe the run range according to subset
#
if ($subset eq "pionwest") {
  $ioff = 35000;
}
elsif ($subset eq "protonwest") {
  $ioff = 36000;
}
elsif ($subset eq "pipmwest") {
  $ioff = 37000;
}
elsif ($subset eq "lambdas") {
  $ioff = 38000;
}
elsif ($subset eq "plambda") {
  $ioff = 39000;
}
else {
  $help = 1;
}

#
# First loop, make directories and copy over input files.
#
print "Preparing input files ... ";
for ($i=$nfirst; $i<=$nlast; $i++) {
  $irun = $ioff + $i;
  $istr = sprintf ("%04d",$i);
  print "$i ";
  $dir = $pisatmp . "/" . $istr;
#  (!-d $dir) || die "Error: attemp to submit overlapping job $istr.";
  system ("mkdir -m 770 -p $dir");

# standard files 
# (Sim3D01.root copied here from /afs/rhic/phenix/software/simulation/)
  system ("cp $pisawrk/phnx.par     $dir");
  system ("cp $pisawrk/gffgo.dat    $dir");
  system ("cp $pisawrk/flukaaf.dat  $dir");
  system ("cp $pisawrk/Sim3D01.root $dir");

# project and subset specific files
  system ("cp $projdir/pisa.kumac   $dir/pisa.kumac");
  system ("cp $projdir/event.par    $dir/event.par");

# write the pisa.input file
  $inputfile = $dir . "/pisa.input";

  $j = $i;

  $istr2 = sprintf ("%04d",$j); 

  $evtfile     = $evtdir . "/" . $subset . "_" . $istr2 . ".oscar";
  if (-f $evtfile) {
    system ("ln -s $evtfile $dir/oscar.input");
  }
  else {
    die "Aborting \nEvent file \"$evtfile\" does not exist.\n";
  }

  open (PISAIN, ">$inputfile");
  print PISAIN "0\n";
  print PISAIN "N\n";
  print PISAIN "0\n";
  print PISAIN "RNDM $j 0\n";
  print PISAIN "RUNN $irun $irun $setnum 0\n";
  print PISAIN "TEXT_FILE\n";
  print PISAIN "ptrig $nruns\n";
  print PISAIN "exit\n";
  close (PISAIN);

  system ("chgrp phenix $dir");
# remove any pisa.out file that already exists here?
#  system("rm $dir/pisa.out");


}
print "\n";

#
# Second loop, submit the jobs
#
print ("Begin job submission ... \n");
for ($i=$nfirst; $i<=$nlast; $i++) {
  $irun = $ioff + $i;
  $istr = sprintf ("%04d",$i);

# Build the options and then the command
  $dir = $pisatmp . "/" . $istr;
  $o   = " -o " . $dir . "/psub.log";
  $r   = " -r " . "ph" . $irun;
  $tM  = " -tM $cpulimit ";
  $c   = " -c \"gps\"";
  
  $cmd = "psub " . # the command
    $tM . " " .      # set the cpu time limit
      $o . " " .       # specify log file
	$r . " " .       # specify job name
	  $c . " " .       # specify constraints
	    $runpisa . " " . # the csh wrapper
	      $dir;            # SESSARGS



    if ($nosubmit) {
       print ("$cmd\n");
   }
    else {
      $output  = `$cmd 2>&1`;
      chomp $output;
      print ("Command: $cmd\n");
      print ("Output:  $output\n");
   }
}

exit;
