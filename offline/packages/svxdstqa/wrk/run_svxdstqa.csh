#!/usr/local/bin/tcsh -f

#set list = `cat dst_s7.list`
#set list = `cat dst_s7_tst.list`
set list = `cat $1`
echo "listfile $1"

set nevt = "10000"

source setup.csh

foreach file ($list)
  set runhead = `basename $file | awk -F"-" '{printf "%d_%04d", $2, $3}'`
  set outfile = outdir/SvxDstQA_${runhead}.root
  set logfile = logdir/runlog_${runhead}.log
#  echo $file "  " $outfile

  echo "Start $runhead " `date`

root -b << EOFF >& $logfile
.x rootsetup.C
gROOT->ProcessLine(".L run_svxdstqa.C+");
run_svxdstqa($nevt, "$file", "$outfile");
.q
EOFF

  echo "End   $runhead " `date`
end
