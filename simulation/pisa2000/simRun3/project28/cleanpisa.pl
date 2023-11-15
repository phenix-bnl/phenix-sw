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
$setname  = "project27";
#$setnum   = "4";
$subset   = "";
$ioff     = 32000;
$nfirst   = 200;
$nlast    = 201;
$move     = 0;

#
# Directory shortcuts (site specific)
#
$simdir   = "/nfs/tmp2/soltz/phenix/simRun2";
$pisadir  = "/nfs/tmp2/soltz/phenix/simRun2/pisa";
$pisainput= "/nfs/tmp2/soltz/phenix/simRun2/pisainput";

GetOptions(
	    "help"       => \$help,
            "move"       => \$move,
            "nfirst=i"   => \$nfirst,
            "nlast=i"    => \$nlast,
	    "ioff=i"     => \$ioff,
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
	  "    --ioff=<i>     Run number offset.\n",
	  "    --subset=<s>   Subset name.\n",
          "    --setname=<s>  Use set different set name.\n",
	  );
    exit;
}

#
# Loop over directories
#
for ($i=$nfirst; $i<=$nlast; $i++) {

# Rename output file
  $istr = sprintf ("%06d",$i);
  $irun = $ioff + $i;
  $irunstr = sprintf ("%010d",$irun);

  $dir = $simdir . "/" . $setname . $subset . "/" . $istr;
  $simfile   = $dir . "/PISAEvent.root";
  $infile    = $dir . "/pisa.input";
  $pisafile  = $pisadir . "/PISA2000_HIJv137-" . $irunstr . "-0027.rootg";
  $pisainput = $pisainput . "/PISA2000_HIJv137-" . $irunstr . "-0027.input";
  if ($move) {
      rename ($simfile,$pisafile);
      rename ($infile,$pisainput);
  }

# Collect job statistics
  $host = `grep Hostname $dir/psub.log`;
  chomp $host;

  $evnt = 0;
  $_ = `grep -c IEVENT $dir/fort.47`;
  chomp;
  $evnt = $_;
  $evntsum += $evnt;
     
  print "$irun   $host   $evnt\n";
 

}
print "Total events: $evntsum\n";

exit;
