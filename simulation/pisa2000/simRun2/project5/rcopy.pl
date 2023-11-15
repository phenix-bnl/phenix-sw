#!/usr/local/bin/perl -w
#
# Description:
#
# Copy remote files to local directory using scp.  The script forms
# hash lists of the local and remote directories, and uses the common and
# difference lists to copy and/or delete files.  This script assumes than
# an ssh-agent is present.

use Env;
use Getopt::Long;
#use File::Basename;
#use File::stat;
#use Net::FTP;
#use Date::Manip;

#
# Set up (read in) default variables and create rcopy directory
#
$src  = "localhost:/nfs/tmp2/$USER/simRun2/project5/pisa";
#$dst  = "rftpexp.rhic.bnl.gov:/phenix/data01/soltz/simRun2/project5/pisa";
$dst  = "rhphemds\@rftpexp.rhic.bnl.gov:/phenix/data19/rhphemds/Run02/simProject05/pisa";
($m1,$d1) = split ":", $src;
($m2,$d2) = split ":", $dst;
print ("Preparing to copy from $m1:$d1 to $m2:$d2 ...\n");

$delete = 0;
$nocopy = 0;
$sshopt = "";
$scpopt = "-c blowfish";
$n      = 2;
$loop   = 1;

$rcopydir = $HOME . "/.rcopy";
if (!-d $rcopydir) {
  print ("Creating $rcopydir to store scp/del scripts.\n");
  system ("mkdir -p $rcopydir");
}

GetOptions ( "help"   => \$help,
	     "push"   => \$push,
	     "nocopy" => \$nocopy,
	     "delete" => \$delete,
	     "sshopt=s" => \$sshopt,
	     "scpopt=s" => \$scpopt,
	     "loop=i" => \$loop,
	     "n=i"    => \$push
	     );

if ($help) {
    print(
	  "Usage: $0 [options] host1:dir1 host2:dir2 \n",
	  "Options:\n",
	  "        --help     Print help message.\n",
	  "        --nocopy   Do not copy files, just write copy scripts.\n",
	  "        --delete   Delete dupilcate files from source.\n",
	  "        --sshopt=s   ssh options to pass.\n",
	  "        --scpopt=s   scp options to pass.\n",
	  "        --loop=i   Number of times to loop.\n",
	  "        --n=i      Number of scp streams to open (default is 1).\n"
	  );
    exit;
}

# Initialize file hashes;
%files1 = ();
%files2 = ();

#
# Read source directory and fill hash.
#
if ($m1 eq "localhost") {
  $lssrc = "ls";
  $scpsrc = $d1;
}
else {
  $lssrc = "ssh " . $sshopt . " " . $m1 . " ls";
  $scpsrc = $src;
}
print ("lssrc = $lssrc\n");
@lines = split "\n", `$lssrc -1s $d1`;
foreach (@lines) {
  if (/(\d+)\s+(\S+)/) {
    $files1{$2}=$1;
  }
}

#
# Read destination directory and fill hash.
#
if ($m2 eq "localhost") {
  $lsdst  = "ls";
  $scpdst = $d2;
}
else {
  $lsdst = "ssh " . $sshopt . " " . $m2 . " ls";
  $scpdst = $dst;
}
print ("lsdst = $lsdst\n");
@lines = split "\n", `$lsdst -1s $d2`;
foreach (@lines) {
  if (/(\d+)\s+(\S+)/) {
    $files2{$2}=$1;
  }
}


#
# Form 1&&2 and 2-1, and then sort 
#
foreach (keys %files1) {
  if (exists $files2{$_}) {
    push (@todelete, $_);
  }
  else {
    push (@tocopy, $_);
  }
}
@tocopy   = sort @tocopy;
@todelete = sort @todelete;

#
# Write out copy/delete scripts, and execute if !$nocopy
#
for ($i=1; $i<=$n; $i++){

  $copyfile = $rcopydir . "/" . "scp.sh" . $i;
  open (SCP,">$copyfile");
  for ($j=$i-1; $j<=$#tocopy; $j+=$n) {
      print SCP "scp $scpopt $scpsrc/$tocopy[$j] $scpdst\n";
    }
  close (SCP);

  $logfile = $rcopydir . "/" . "scp.log" . $i;
  if (!$nocopy) {
    print ("Starting $copyfile\n");
    system ("source $copyfile > $logfile 2>&1 &");
  }
}

$delfile  = $rcopydir . "/" . "del.sh";
open (DEL,">$delfile");
foreach (@todelete) {
  print DEL "rm $m1:$d1/$_\n";
}
close (DEL);

exit 0;
