#!/bin/bash

ITERATION=${1:?first arg as iteration number}
source $(dirname $0)/common.sh

SetPi0HistEnv $ITERATION

echo doing iteration $ITERATION

root -l $MACRO_DIR/check_calib_result.cc\($ITERATION\)