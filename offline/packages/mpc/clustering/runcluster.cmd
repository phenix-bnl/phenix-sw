#!/bin/bash

echo directory is $PWD
echo $HOSTNAME
echo $LD_LIBRARY_PATH

# for mpc cluster file generation
let line=$1+1
run=`sed -n "${line}p" runs.list`

#root.exe -b -q mpc_cluster.C\($run,100\)	# for only doing the first 10K events

#root -q mpc_cluster.C\($run,100\)	# for visual checking of clustering

root.exe -b -q mpc_cluster.C\($run\)	# for running on the whole file


