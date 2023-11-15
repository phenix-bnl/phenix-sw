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
$cpulimit = "12:00";
$setname  = "project28";
$setnum   = "28";
$subset   = "";
$ioff     = 32000;
$nosubmit = 0;
$nohpss   = 1;
$nruns    = 1000;
$nfirst   = 200;
$nlast    = 299;
$iseed    = 1;

#
# Directory shortcuts (site specific)
#
$simdir   = "/nfs/tmp2/soltz/phenix/simRun2";
$pisawrk  = "/usr/gapps/phenix/cvs/simulation/pisa2000/wrk";
$projdir  = "/usr/gapps/phenix/cvs/simulation/pisa2000/simRun3/" . $setname;
$runpisa  = "$projdir" . "/runpisa.csh";

GetOptions(
	    "help"       => \$help,
            "nfirst=i"   => \$nfirst,
            "nlast=i"    => \$nlast,
            "nruns=i"    => \$nruns,
	    "iseed=i"    => \$iseed,
	    "ioff=i"    => \$ioff,
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
	  "    --ioff         Run number offset.\n",
	  "    --nosubmit     Do not submit jobs.\n",
	  "    --nohpss       Do not copy files off to storage.\n",
          "    --cpulimit=<s> cpu limit string provided to psub (-tm).\n",
          "    --setname=<s>  Use set different set name.\n",
          "    --subset=<s>   Set the subset letter.\n"
	  );
    exit;
}

#
# First loop, make directories and copy over input files.
#
print "Preparing input files ... ";
for ($i=$nfirst; $i<=$nlast; $i++) {
  $irun = $ioff + $i;
  $istr = sprintf ("%06d",$i);
  print "$i ";
  $dir = $simdir . "/" . $setname . "/" . $istr;
#  (!-d $dir) || die "Error: attemp to submit overlapping job $istr.";
  mkdir ($dir, 0770);

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

  $j = $i-200;
  $istr2 = sprintf ("%06d",$j);
  $hfile     = "../../hijing/dauminbi200/hijingzeb_" . $istr2 . ".out";


  open (PISAIN, ">$inputfile");
  print PISAIN "0\n";
  print PISAIN "N\n";
  print PISAIN "0\n";
  print PISAIN "RNDM $j 0\n";
  print PISAIN "RUNN $irun $irun $setnum 0\n";
  print PISAIN "hijing 40 $hfile\n";
  print PISAIN "ptrig $nruns\n";
  print PISAIN "exit\n";
  close (PISAIN);

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
  $istr = sprintf ("%06d",$i);

# Build the options and then the command
  $dir = $simdir . "/" . $setname . "/" . $istr;
  $o   = " -o " . $dir . "/psub.log";
  $r   = " -r " . "ph" . $irun;
  $tM  = " -tM $cpulimit ";
  $c   = " -c \"gps|tera\" ";
  
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
