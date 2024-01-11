#!/bin/bash

#
# usage: runcalpi0.sh <n>
#
#        n is the line number of the dst in f.list

#NEVENTS=500
NEVENTS=0

MAXITER=8
#MAXITER=2

SIMFLAG=1

let line=$1+1
FILENAME_FULL=`sed -n "${line}p" f.list`

mkdir ${FILENAME_FULL%.list}
cd ${FILENAME_FULL%.list}
ln -s ../MpcCal.recal_gains_0 .

if [[ $SIMFLAG -eq 1 ]]
then
  RUNNUM=0
else
  RUNNUM=205249
fi

ITER=1
while [[ $ITER -le $MAXITER ]]
do
  if [[ $SIMFLAG -eq 1 ]]
  then
    OUTFILE="pi0cal_${ITER}.root"
  else
    OUTFILE="pi0cal_${RUNNUM}_${ITER}.root"
  fi


  echo "****** Iteration $ITER ******"

  echo root.exe -q -b run_pi0cal_clustering.C\(\"${OUTFILE}\",\"${FILENAME_FULL}\",${RUNNUM},${ITER},${NEVENTS},${SIMFLAG}\)
  root.exe -q -b ../run_pi0cal_clustering.C\(\"${OUTFILE}\",\"../${FILENAME_FULL}\",${RUNNUM},${ITER},${NEVENTS},${SIMFLAG}\)

  let ITER=${ITER}+1

done

