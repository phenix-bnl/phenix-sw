#!/bin/bash

ITER_BEF=0
ITER_AFT=${1:?need a iteration number}

source $(dirname $0)/common.sh

SetPi0HistEnv $ITER_BEF
export FNAME_ROOT_BEF=$PI0_HIST_ROOT_FILE_NAME
export FNAME_TABLE_BEF=$PI0_HIST_TABLE_FILE_NAME_TOWER

SetPi0HistEnv $ITER_AFT
export FNAME_ROOT_AFT=$PI0_HIST_ROOT_FILE_NAME
export FNAME_TABLE_AFT=$PI0_HIST_TABLE_FILE_NAME_TOWER

export FNAME_COEF=$COEF_FILE_NAME_IN ## not _OUT
export FNAME_COEF_SUPERMOD=$COEF_FILE_NAME_SUPERMOD_IN ## not _OUT

export FNAME_UNCALIB_LIST=$UNCALIB_LIST_FILE_NAME_IN ## not _OUT

export FNAME_COEF_RUN5=$PI0_HIST_DIR_FOR_CALIB/coefficient_run5.txt

root -b <<EOF
.L $MACRO_DIR/summarize_calib.cc
summarize_calib();
EOF
