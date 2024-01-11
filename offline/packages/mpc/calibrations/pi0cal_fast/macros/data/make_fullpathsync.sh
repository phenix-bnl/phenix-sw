#!/bin/bash


function cleanup
{
    if [ -f $1 ];
    then
	rm $1
    fi
}

# $1 = filename containing list of runs
# create fullpathsync.list which contains 2 lines: 1 for DST_MPC, 1 for DST_EVE

dir1=/common/p8/phnxmpc/dst
DST_TYPE=( DST_MPC DST_EVE )

#pi0cal_fast/macros/run9ppMinBiasDisk/sync/DST_MPC_MB_run9pp_500GeV_pro83.index.list

LISTDIR=/home/phnxmpc/scott/offline/packages/mpc/calibrations/pi0cal_fast/macros/run11ppMinBiasFastProd
rm -rf $LISTDIR
mkdir -p $LISTDIR/sync
mkdir -p $LISTDIR/shortLists

#lines of fullpathpsync.list are directories in $LISTDIR/sync

cleanup fullpathsync.list

for dst_type in "${DST_TYPE[@]}"
do
  echo $LISTDIR/sync/${dst_type}_run11pp_500GeV.list
  echo $LISTDIR/sync/${dst_type}_run11pp_500GeV.list >>  fullpathsync.list

  mkdir -p $LISTDIR/shortLists/${dst_type}
  cleanup $LISTDIR/sync/${dst_type}_run11pp_500GeV.list

  for run_list in $*
  do
    for run in `cat $run_list`
      do
      echo $LISTDIR/shortLists/${dst_type}/0000${run}.list
      echo $LISTDIR/shortLists/${dst_type}/0000${run}.list >> $LISTDIR/sync/${dst_type}_run11pp_500GeV.list
      #/common/p8/phnxmpc/dst/run_0000330000_0000331000/DST_EVE/
      let "runlow = run/1000"
      let "runhi = runlow + 1"
      subdir=run_0000${runlow}000_0000${runhi}000
      #echo ${dir1}/${subdir}/DST_EVE
      cleanup $LISTDIR/shortLists/${dst_type}/${run}.list
      ls ${dir1}/${subdir}/${dst_type}/*${run}*.root >> $LISTDIR/shortLists/${dst_type}/0000${run}.list
    done
  done
done

exit


#run9 500
#280186
#/home/phnxmpc/condor_example/reco_macros/run_0000280000_0000281000/DST_EVE/DST_EVE_run11pp_mpc-0000280186-0006.root

#run9 200
#286194
#/home/phnxmpc/condor_example/reco_macros/run_0000286000_0000287000/DST_EVE/DST_EVE_run11pp_mpc-0000286194-0036.root

#run11 500
#/common/s2/Subsystems/phnxmpc/dst/run_0000327000_0000328000

dir1=/common/s2/Subsystems/phnxmpc/dst
dir2=/home/phnxmpc/condor_example/reco_macros/

runlow=0
runhi=0



initdir=$PWD

MPC_LIST=${initdir}/DST_MPC_run_${run}.list
EVE_LIST=${initdir}/DST_EVE_run_${run}.list

for dir in $dir1 $dir2
do
  if [ -d $dir/${subdir}/DST_MPC/ ];
  then
      cd $dir
      echo "found data you want in $dir1/$subdir/DST_MPC"
      ls  $dir/$subdir/DST_MPC/*${run}*  > $MPC_LIST
      ls  $dir/$subdir/DST_EVE/*${run}*  > $EVE_LIST
      cd $initdir
  fi
done

cd $initdir
run $nevents $MPC_LIST $EVE_LIST

rm -f $MPC_LIST
rm -f $EVE_LIST
