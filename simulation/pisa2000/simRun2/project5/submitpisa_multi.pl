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
$cpulimit = "50:00";
$setname  = "project5";
$setnum   = "5";
$subset   = "pionwest";
$nosubmit = 0;
$nohpss   = 1;
$nruns    = 50000;
$nfirst   = 101;
$njob     = 2;
$npercpu  = 10;
$iseed    = 1;

GetOptions(
	    "help"       => \$help,
            "nfirst=i"   => \$nfirst,
            "njob=i"    => \$njob,
            "nruns=i"    => \$nruns,
	    "iseed=i"    => \$iseed,
	    "nosubmit"   => \$nosubmit,
	    "nohpss"     => \$nohpss,
	    "cpulimit=s" => \$cpulimit,
	    "setname=s"  => \$setname,
	    "subset=s"   => \$subset,
	    "npercpu=i"    => \$npercpu
	   );

if ($help) {
    print(
	  "Usage: $0 [options] \n",
	  "Options:\n",
	  "    --help         Print help message.\n",
	  "    --nfirst       First job (file number).\n",
	  "    --njob        Last job (file number).\n",
	  "    --nruns        Number of events per file.\n",
	  "    --iseed        Grndm starting cycle.\n",
	  "    --nosubmit     Do not submit jobs.\n",
	  "    --nohpss       Do not copy files off to storage.\n",
          "    --cpulimit=<s> cpu limit string provided to psub (-tm).\n",
          "    --setname=<s>  Use set different set name.\n",
          "    --subset=<s>   Set the subset letter.\n",
          "    --npercpu      Number of consecutive pisa runs per batch job.\n"
	  );
    exit;
}

#
# Directory shortcuts (site specific)
#
$evtdir   = "/nfs/tmp2/$USER/simRun2/" . $setname . "/oscar";
$pisatmp  = "/nfs/tmp2/$USER/simRun2/" . $setname . "/pisatmp/" . $subset;
$multidir = "/nfs/tmp2/$USER/simRun2/" . $setname . "/pisatmp/multi";
$pisawrk  = "/usr/gapps/phenix/cvs/simulation/pisa2000/wrk";
$projdir  = "/usr/gapps/phenix/cvs/simulation/pisa2000/simRun2/" . $setname;

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
print "Start loop over jobs ... ";
system ("mkdir -m 770 -p $multidir");
for ($j=0; $j<$njob; $j++) {

# open file for multi-run pisa sumbission
  $multifile = $multidir . "/multijob_" . $j . ".csh";
  open (MULTIN, ">$multifile");
  print MULTIN "#!/bin/csh\n";
  print MULTIN "# PSUB -b phenix    # Use phenix bank\n";
  print MULTIN "# PSUB -eo          # Direct stderr to stdout\n";
  print MULTIN "# PSUB -s /bin/csh  # Use csh\n";
  print MULTIN "# PSUB -me          # Send mail at end\n";
  print MULTIN "echo \"Hostname: `hostname`\"\n";
  print MULTIN "echo \"OS: `uname -a`\"\n";
  print MULTIN "echo \"Date: `date`\"\n";
  print MULTIN "source /usr/gapps/phenix/setup/phenix_setup.csh\n";
  print MULTIN "cd $multidir\n";

  for ($k=0; $k<$npercpu; $k++) {
    $i = $nfirst + ($j*$npercpu) + $k;
    $irun = $ioff + $i;
    $istr = sprintf ("%04d",$i);
    print "$istr ";

# add to multi-run commands
    print MULTIN "cd ../$istr\n";
    print MULTIN "pisa < pisa.input >& pisa.out\n";
    print MULTIN "chgrp phenix *\n";
    print MULTIN "chmod g+rw *\n";
    print MULTIN "echo \"Date: `date`\"\n";

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
    $evtfile     = $evtdir . "/" . $subset . "_" . $istr . ".oscar";
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

# close multifile
  close (MULTIN);

# build command and submit job
  print "\n";

  # Build the options and then the command
  $o   = " -o " . $multidir . "/multijob_" . $j . ".log";
  $r   = " -r " . "pisa" . $j;
  $tM  = " -tM $cpulimit ";
  $c   = " -c \"gps\"";
  $cmd = "psub " . # the command
      $tM . " " .      # set the cpu time limit
	$o . " " .       # specify log file
	  $r . " " .       # specify job name
	    $c . " " .       # specify constraints
	      $multifile;      # the multi-job pisa script

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
