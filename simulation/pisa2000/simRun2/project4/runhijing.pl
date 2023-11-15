#!/usr/local/bin/perl -w
#
# Description: 
# Generate hijing135 events.
#

use Env;
use Getopt::Long;

#
# Set basic defaults
# 
$nfirst = 0;
$hdir = "/home/phenix/hijing/auauperiph200";

#
# Set run (these are the hijing.par options).
# All are self-explanatory, except for $istep = nx1M seeds to skip
# between consecutive jobs.
#
$nfirst = 1;
$nlast  = 10;
$nruns  = 500;
$elgev  = 200.0;
$ref    = "\'CMS\'";
$proj   = "\'A\'";
$targ   = "\'A\'";
$n1     = 197;
$iz1    = 79;
$n2     = 197;
$iz2    = 79;
$bmin   = 10.86;
$bmax   = 20.0;
$iseed  = 0;
$iseq   = 1;
$istep  = 0;
$jet_trigger = 0;
$pthard = 0.0;

GetOptions (
             "nfirst=i" => \$nfirst,
	     "nlast=i"  => \$nlast,
	     "nruns=i"  => \$nruns,
	     "iseed=i"  => \$iseed,
	     "iseq=i"  => \$iseq,
	     "istep=i"  => \$istep,
             "bmin=f"   => \$bmin,
             "bmax=f"   => \$bmax,
	     "help"      => \$help
	     );

if ($help) {
    print(
	  "Usage: $0 [options] \n",
	  "Options:\n",
	  "        --nfirst   First job (file number).\n",
	  "        --nlast    Last job (file number).\n",
	  "        --nruns    Number of events per file.\n",
	  "        --bmin     Minimum impact parameter.\n",
	  "        --bmax     Maximum impact parameter.\n",
	  "        --iseed    Grndm starting cycle.\n",
	  "        --iseq     Amount to increment sequence between successfive jobs.\n",
	  "        --istep    Millions of seeds between successive jobs.\n",
	  "        --help     Print help message.\n"
	  );
    exit;
}

#
# Form working directory from input
#
chdir "$hdir";

#
# Loop over njobs = nlast - nfirst
#

for ($i=$nfirst; $i<=$nlast; $i++) {
$iseed = $iseed + $iseq;
$iseed_skip = ($i-1) * $istep;
#
# Write out hijing.par file
#
open (PAR,">hijing.par");
print PAR " \$hijing_inp\n";
print PAR "  nruns = $nruns\n";
print PAR "  elgev = $elgev\n";
print PAR "  ref = $ref\n"; 
print PAR "  chproj = $proj\n"; 
print PAR "  chtarg = $targ\n"; 
print PAR "  n1 = $n1\n"; 
print PAR "  iz1 = $iz1\n";
print PAR "  n2 = $n2\n"; 
print PAR "  iz2 = $iz2\n";
print PAR "  bmin = $bmin\n"; 
print PAR "  bmax = $bmax\n"; 
print PAR "  iseed = $iseed\n"; 
print PAR "  iseed_skip = $iseed_skip\n"; 
print PAR "  jet_trigger = $jet_trigger\n"; 
print PAR "  pthard = $pthard\n"; 
print PAR " \$end\n";
close (PAR);

#
# Execute hijing and rename output file
#

$istr = sprintf ("%06d",$i);
$hout = "hjievt135_500auauperiph200sq_" . $istr . ".dat";
$hlog = "hjievt135_500auauperiph200sq_" . $istr . ".log";
$hpar = "hjievt135_500auauperiph200sq_" . $istr . ".par";;

system ("hijing135 > $hlog");
rename ("hijingzeb.out", $hout);
rename ("hijing.par", $hpar);

}
exit;
