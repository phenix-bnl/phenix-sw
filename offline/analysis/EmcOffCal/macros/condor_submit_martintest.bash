#! /bin/bash
runlist=/direct/phenix+u/workarea/manion/work/EMC_Calib/runlist_4x4c_martin.txt
for run in `cat $runlist`
  do  
  rm -r /phenix/spin/phnxsp01/manion/martin_test/$run
  mkdir /phenix/spin/phnxsp01/manion/martin_test/$run
  condor_submit \
  -a "output = /direct/phenix+u/workarea/manion/work/EMC_Calib/cout/$run.out" \
  -a "error = /direct/phenix+u/workarea/manion/work/EMC_Calib/cerr/$run.err" \
  -a "Log = /direct/phenix+u/workarea/manion/work/EMC_Calib/log/myjob.log.$run" \
  -a "Arguments = $run" \
  -a "Initialdir = /phenix/spin/phnxsp01/manion/martin_test/$run" \
  /direct/phenix+u/manion/condor/EMC_Calib.job 
done
