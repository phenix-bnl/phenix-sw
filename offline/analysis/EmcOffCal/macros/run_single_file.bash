#! /bin/bash

run_tmp=$1

root -b -q -l /direct/phenix+u/workarea/manion/source/EMC_Calib/macros/EMC_CalibAna.C\(\"/direct/phenix+u/workarea/manion/work/EMC_Calib/seglists/"$run_tmp"_d.list\","$run_tmp"\)
    
echo finished $run_tmp !

