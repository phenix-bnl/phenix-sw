#! /bin/bash
runlist=/direct/phenix+u/workarea/manion/work/EMC_Calib/runlist_4x4c_sel.txt
for run in `cat $runlist`
  do  
  rm -r /phenix/analysis/phnxreco/run11emccal/EMC_Calib/$run
  mkdir /phenix/analysis/phnxreco/run11emccal/EMC_Calib/$run
  condor_submit \
  -a "output = /direct/phenix+u/workarea/manion/work/EMC_Calib/cout/$run.out" \
  -a "error = /direct/phenix+u/workarea/manion/work/EMC_Calib/cerr/$run.err" \
  -a "Log = /direct/phenix+u/workarea/manion/work/EMC_Calib/log/myjob.log.$run" \
  -a "Arguments = $run" \
  -a "Initialdir = /phenix/analysis/phnxreco/run11emccal/EMC_Calib/$run" \
  /direct/phenix+u/manion/condor/EMC_Calib.job 
done
