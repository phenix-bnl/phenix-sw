#! /usr/local/bin/perl

use Env;
use File::Basename;

$cmdlist = "ndstlist";
$rejlist = "/phenix/workarea/isobe/run4ana/reject_run.lst";
$outpath = "/phenix/data24/isobe/Run4/pi0qa/";
$num = 20;

## assumes nothing in the path has a "-" in it but the filename
## which itself has two "-"'s in it: one before the runnumber and
## the second between the runnumber and the seg.

$catruncomm = "cat $cmdlist | sort | uniq"; 
@runlist = qx/$catruncomm/;
$catruncomm = "cat $cmdlist | awk -F - \'{print \"pi0QA_\"\$2\"-\"\$3}\' | sort | uniq"; 
@outputlist = qx/$catruncomm/;
print "found " . $#runlist . " runs from $cmdlist e.g. " . $runlist[0]; 
@finallist = ();

$catrejcomm = "cat $rejlist | sort | uniq"; 
@rejrun = qx/$catrejcomm/;

$i = 0;

foreach $file (@runlist) {

    $i++;

    chomp($file);
    $outputfile = $file;
    $outputfile =~ s/(.+?)(\-)(.+)/pi0QA_$3/g;
    $outputfile = $outpath . $outputfile;

    $cmd0 = ".x run_multifiles.C(\"$file\", 0 ,\"$outputfile\");";
#    print("$cmd0\n");
    
    $batchfile = "batch${i}.csh";
    open(BATCH,"> $batchfile");
    print BATCH "#!/usr/local/bin/tcsh \n";
    print BATCH "\n";
    print BATCH "source /opt/phenix/bin/phenix_setup.csh pro\n";
    print BATCH "source /phenix/u/isobe/setup_own.csh \n";
#    print BATCH "setenv LD_LIBRARY_PATH /phenix/workarea/isobe/run4ana/install/lib:\$LD_LIBRARY_PATH \n";
#    print BATCH "setenv OO_FD_BOOT $PHENIX_FD_BOOT \n";
    print BATCH "\n";
    print BATCH "cd " . $ENV{"PWD"} . "\n";
    print BATCH "\n";
    print BATCH 'root.exe -b <<EOF' . "\n";

    print BATCH " $cmd0 \n";
    print BATCH '.q' . "\n";
    print BATCH 'EOF' ."\n";
    print BATCH "\n";
    close(BATCH);
    
}    

