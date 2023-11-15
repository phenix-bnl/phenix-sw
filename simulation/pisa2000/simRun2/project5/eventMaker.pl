#!/usr/local/bin/perl
#
# Perl shell wrapper to call eventMaker.C with a loop over 
# files using consecutive seeds.
#
use Env;
use Getopt::Long;

GetOptions ( "help"   => \$help
	     );

#
# Some defaults and input parsing
#
$nevts = 50000;
$seed1 = 12345;
$seed2 = 54321;
$nfile = 1000;

$name = @ARGV[0];

if ($help) {
    print(
	  "Usage: $0 [options] [name]\n",
	  "Options:\n",
	  "        --help         Print help message.\n",
          "        name - Base name for input/output files passed to eventMaker.C\n"
	  );
    exit;
}

# Loop through the files
for ($i=1;$i<=$nfile;$i++) {
  $infile  = $name . ".input";
  $outfile = $name . "_" . sprintf("%04d",$i) . ".oscar";
  $cmd = "$PHROOT/cvs/simulation/pisa2000/simRun2/project5/eventMaker $infile $outfile $nevts $seed1 $seed2";

  print "Executing: $cmd\n";
  $output = `$cmd 2>&-`;
  print "$output\n";
  $output =~ /Seeds:\s(\d+)\s(\d+)/;
  $seed1 = $1;
  $seed2 = $2;
}

exit;
