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
$setname  = "project5";
$subset   = "protonwest";
$nfirst   = 101;
$nlast    = 300;
$move     = 1;
$nostat   = 1;

#
# Directory shortcuts (site specific)
#
$simdir   = "/nfs/tmp2/$USER/simRun2";

GetOptions(
	    "help"       => \$help,
            "move"       => \$move,
            "nostat"     => \$nostat,
            "nfirst=i"   => \$nfirst,
            "nlast=i"    => \$nlast,
            "subset=s"   => \$subset,
	    "setname=s"  => \$setname);

if ($help) {
    print(
	  "Usage: $0 [options] \n",
	  "Options:\n",
	  "    --help         Print help message.\n",
	  "    --move         Move PISAEvent.root and pisa.input files.\n",
	  "    --nfirst=<i>   First job (file number).\n",
	  "    --nlast=<i>    Last job (file number).\n",
	  "    --subset=<s>   Subset name.\n",
          "    --setname=<s>  Use set different set name.\n",
	  );
    exit;
}

#
# Fix the run range according to subset
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
# Loop over directories
#
for ($i=$nfirst; $i<=$nlast; $i++) {

# Rename output file
  $istr = sprintf ("%04d",$i);
  $irun = $ioff + $i;
  $irunstr = sprintf ("%010d",$irun);

  $tmpdir    = $simdir . "/" . $setname . "/pisatmp/" . $subset . "/" . $istr;
  $simfile   = $tmpdir . "/PISAEvent.root";
  $pisadir   = $simdir . "/" . $setname . "/pisa/";
#  $infile    = $tmpdir . "/pisa.input";
  $pisafile  = $pisadir . "/PISA2000_OSCAR01-" . $irunstr . "-0005.rootg";
#  $pisainput = $pisainput . "/PISA2000_OSCAR01-" . $irunstr . "-0005.input";
  if ($move) {
    print "rename $simfile $pisafile\n";
      rename ($simfile,$pisafile);
#      rename ($infile,$pisainput);
  }

# Collect job statistics
  if (!$nostat) {
    $host = `grep Hostname $tmpdir/psub.log`;
    chomp $host;
    
    $evnt = 0;
    $_ = `grep -c "Number remaining      0" $tmpdir/pisa.out`;
    chomp;
    $evnt = $_;
    $evntsum += $evnt;
    
    print "$irun   $host   $evnt\n";
  }
 

}
if (!$nostat) {
  print "Total events: $evntsum\n";
}

exit;
