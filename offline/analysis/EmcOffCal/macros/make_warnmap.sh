#!/bin/bash

source $(dirname $0)/common.sh

CURRENT_DIR=$(pwd)
RUN_DATA_LIST=$CURRENT_DIR/autogen_list/run_data_list_for_make_warnmap.txt
mkdir -p $CURRENT_DIR/autogen_list

#find $TREE_DIR -name 'Nhits_*.root' |\
#    sed 's/^\(.*\/*EMCAL-0000\([0-9]*\).*.root$\)/\2  \1/g' >$RUN_DATA_LIST
#find $TREE_DIR -name 'Nhits_*.root' |\
#    sed 's/^\(.*\/*ana129-0000\([0-9]*\).*.root$\)/\2  \1/g' >$RUN_DATA_LIST

#find $TREE_DIR -name 'nhit-*.root' |\
#    sed 's/^\(.*\/*nhit-\([0-9]*\).*.root$\)/\2  \1/g' >$RUN_DATA_LIST

#EmcOffCal_ClustNT_303445_0.root_nhit.root
#find $TREE_DIR -name 'EmcOffCal_ClustNT_*.root_nhit.root' |\
#    grep -v crashed | grep -v backup | \
#    sed 's/^\(.*\/*EmcOffCal_ClustNT_\([0-9]*\)_\([0-9]\).root_nhit.root$\)/\2  \1/g' > $RUN_DATA_LIST

find $TREE_DIR -name 'Hitfile.root' |\
    grep -v crashed | grep -v backup |\
sed 's/^\(.*\/*\/\([0-9]*\)\/Hitfile.root$\)/\2  \1/g' > $RUN_DATA_LIST

#echo 'find TREE_DIR'
#find $TREE_DIR -name 'EmcOffCal_ClustNT_*.root_nhit.root' | grep -v crashed | grep -v backup

mkdir -p $WARNMAP_DIR
echo 'cat $RUN_DATA_LIST = '$RUN_DATA_LIST
cat $RUN_DATA_LIST

root.exe -b <<EOF
.L $MACRO_DIR/make_warnmap.cc
make_warnmap("$RUN_DATA_LIST");
EOF

#