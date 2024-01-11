#!/bin/bash

NUMBER=${1:?2nd arg must be an iteration number}
source $(dirname $0)/common.sh

CURRENT_DIR=`pwd`
RUN_DATA_LIST=$CURRENT_DIR/autogen_list/run_data_list_for_make_pi0_hist_$NUMBER.txt
mkdir -p $CURRENT_DIR/autogen_list

SetPi0HistEnv $NUMBER

#find $TREE_DIR -name 'Clusters_*.root' >$RUN_DATA_LIST
#find $TREE_DIR -name 'cluster_*.root' >$RUN_DATA_LIST
#for tmporary
#RUN_DATA_LIST_0=$CURRENT_DIR/autogen_list/run_data_list_for_make_pi0_hist_0.txt
#cp $RUN_DATA_LIST_0 $RUN_DATA_LIST
#find $TREE_DIR -name 'cluster-*.root' >$RUN_DATA_LIST

find $TREE_DIR -name 'Clusterfile*.root' | grep -v crashed  | grep -v backup >$RUN_DATA_LIST

mkdir -p $PI0_HIST_DIR
cat $RUN_DATA_LIST

root -b <<EOF
.L $MACRO_DIR/make_pi0_hist.cc
make_pi0_hist_iter($NUMBER, "$RUN_DATA_LIST")
EOF

#
