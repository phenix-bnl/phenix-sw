#! /bin/bash

runlist=/direct/phenix+u/workarea/manion/work/EMC_Calib/runlist_4x4c_sel.txt
for run in `cat $runlist`
  do
  mkdir $run
  cd $run
  root -b -q -l /direct/phenix+u/workarea/manion/source/EMC_Calib/macros/EMC_CalibAna.C\(\"/direct/phenix+u/workarea/manion/work/EMC_Calib/seglists/"$run"_d.list\","$run"\)
  echo finished!
  cd ..
done
