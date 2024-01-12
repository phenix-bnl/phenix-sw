#! /usr/local/bin/perl

use Env;
use File::Basename;

$cmdlist = "ndstlist_isobe";
$rejlist = "/phenix/workarea/isobe/run4ana/reject_run.lst";
$outpath = "/phenix/data24/isobe/Run4/pi0reco/20040914_WO_ESRecal/";
$num = 20;

## assumes nothing in the path has a "-" in it but the filename
## which itself has two "-"'s in it: one before the runnumber and
## the second between the runnumber and the seg.

$catruncomm = "cat $cmdlist | awk -F - \'{print \$2+0}\' | sort | uniq"; 
@runlist = qx/$catruncomm/;
print "found " . $#runlist . " runs from $cmdlist e.g. " . $runlist[0]; 
@finallist = ();

$catrejcomm = "cat $rejlist | sort | uniq"; 
@rejrun = qx/$catrejcomm/;

foreach $run (@runlist) {

    $flag = 0;
    foreach $rej (@rejrun) {
	if( $run eq $rej ){
	    $flag = 1;
	    last;
	}
    }

    if( $run >= 109016 && $run <= 109187 ){$flag = 1;}# Centrality unstable
    if( $run >= 110237 && $run <= 111364 ){$flag = 1;}# 1.7% conv. run
    if( $run >= 120581 && $run <= 121112 ){$flag = 1;}# 8.5% conv. run

    if($flag == 1){
	next;
    }

    chomp($run);
    $runfilename = $run . ".list";
    $listcomm = "grep " . $run . " " . $cmdlist ." > ". $runfilename;
    qx/$listcomm/;
    @runcont = `cat $runfilename`;
    if ($#runcont > $num) {
	$splitcom = "split -l $num $runfilename " . $runfilename . "_";
	qx/$splitcom/;
	`rm $runfilename`;
#	print "$runfilename\n";
	@templist = `ls $runfilename*`;
	@finallist = (@finallist, @templist);
    }
    else {
	@finallist = (@finallist, $runfilename);
    }
}


$i = 0;

foreach $listfile (@finallist) {

    $i++;

    chomp($listfile);
        
    $listfile =~ m/(.*)\.list(.*)/;
    $outfilestub = $1 . $2;
    
    $ofile = $outpath . "/gamma-" . $outfilestub . ".root";

    $ifile = $listfile;

    $cmd0 = ".x run_multifiles.C(\"$ifile\", 0 ,\"$ofile\");";
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

