#!/bin/csh -f
count_events=0
count_goodevents=0
group="g10"
cat $1 |
    while read line; do
    run_number=${line}
    nevents=`psql daq -c "select scalerupdatescaled from trigger where runnumber=${run_number} and name='BBCLL1';" | grep -v scale | grep -v \- | grep -v row`
    if [ $run_number -ge 107445 -a $run_number -le 107827 ]
      then
         group="g1"
    fi
    if [ $run_number -ge 108280 -a $run_number -le 108714 ]
      then
         group="g2"
    fi
    if [ $run_number -ge 108769 -a $run_number -le 110236 ]
      then
         group="g3"
    fi
    if [ $run_number -ge 110237 -a $run_number -le 111031 ]
      then
         group="conv1"
    fi
    if [ $run_number -ge 111032 -a $run_number -le 111592 ]
      then
         group="g4"
    fi
    if [ $run_number -ge 111604 -a $run_number -le 113564 ]
      then
         group="g5"
    fi
    if [ $run_number -ge 113570 -a $run_number -le 114330 ]
      then
         group="g6"
    fi
    if [ $run_number -ge 114331 -a $run_number -le 116691 ]
      then
         group="g7"
    fi
    if [ $run_number -ge 116707 -a $run_number -le 117852 ]
      then
         group="g8"
    fi
    if [ $run_number -ge 118264 -a $run_number -le 120849 ]
      then
         group="g9"
    fi
    if [ $run_number -ge 120845 -a $run_number -le 121111 ]
      then
         group="conv2"
    fi
    if [ $run_number -ge 121220 -a $run_number -le 122223 ]
      then
         group="g10"
    fi
    hasqa=`psql -h phnxdb2.phenix.bnl.gov calibrations -c "select count(runnumber) from electronqa where runnumber=${run_number} and tag like '%pro59%';" | grep -v count | grep -v \- | grep -v row`
    isgood=`qa_check -f condition_${group} ${run_number}`
    echo ${run_number} ${group} ${nevents} ${hasqa} ${isgood} | awk '{printf("%d   %5s   %10d    %5d      %d\n",$1,$2,$3,$4,$5);}'
done

