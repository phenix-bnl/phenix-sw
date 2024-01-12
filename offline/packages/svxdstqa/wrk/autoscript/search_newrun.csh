#!/usr/local/bin/tcsh -f

#if ( -f /opt/phenix/bin/phenix_setup.csh ) then
#  source /opt/phenix/bin/phenix_setup.csh
#endif

#set path = ( $path /opt/condor/bin )



######################
# moving to working dir 
set autojobdir = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/autoscript"
cd $autojobdir

set logfile = $autojobdir/search_newrun.log

if (-f $logfile) then
    rm $logfile
endif

echo "Start search_newrun.csh" >>& $logfile

source /opt/phenix/bin/phenix_setup.csh

echo "end source phenix_setup.csh" >>& $logfile

###check first running jobs
set njobs = `/opt/condor/bin/condor_q phnxvtx | grep merge_and_analysis | wc -l`


@ njobs = $njobs - 1

set maxjobs = 0
if ($njobs > $maxjobs) then
    echo "No. of jobs running $njobs is still large. Exiting.." >>& $logfile
    exit;
endif

######################
# get runlistfile
set dirnamelist = (\
  "s7" \
  "s8" \
)

#
set runlistfiles = ""

foreach dirA ($dirnamelist)
  set dirAA = `printf "/common/%s/phnxreco/run11_autovtx_online" $dirA`
  #echo $dirAA
  set dirrunlist = `ls -d $dirAA/run_0000*_0000*`
  foreach dirrun ($dirrunlist)
    set dirrunname = ${dirA}_`basename $dirrun`.list
    set runlistfiles = "$runlistfiles $dirrunname"
#    echo $dirrunname
    if (-f $dirrunname) then
	rm $dirrunname
    endif
    ls $dirrun/SvxDstQA/*.root > $dirrunname

    echo "end creating $dirrunname" >>& $logfile
  end
end

echo "end creating runlistfiles" >>& $logfile
#echo "runlistfiles = $runlistfiles" >>& $logfile

######################
# check if the run is already processed
# then only the run (not processed) are processed

set newrunlist = "newrunfile.list"
if( -f $newrunlist) then
  rm $newrunlist
endif

set badruns=()
set goodruns=()

#check first all run numbers and reject with unfinished flags
foreach runlist ($runlistfiles)
  set filelist = `cat $runlist`
  foreach datafile ($filelist)
    set runnum = `basename $datafile .root | awk -F'-' '{printf "%d", $2}'`

#    echo "runnum = $runnum" >>& $logfile
    set knowngood = 0
    foreach goodrun ($goodruns)
       if ($goodrun == $runnum) then
	   set knowngood = 1
           break
       endif
    end
    if ($knowngood == 1) then
	continue
    endif 

    set knownbad = 0
    foreach badrun ($badruns)
       if ($badrun == $runnum) then
	   set knownbad = 1
           break
       endif
    end
    if ($knownbad == 1) then
	continue
    endif 

#check new run
#    echo "start readDB_checkDST.pl for $runnum" >>& $logfile

    set stat = `/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/autoscript/readDB_checkDST.pl $runnum`
    echo "end readDB_checkDST.pl for $runnum" >>& $logfile

    if (($stat == 3)||($stat == -3)) then
        set goodruns = ($goodruns $runnum)
    else
        set badruns = ($badruns $runnum)
    endif 
  end
end

#echo "Runs with DST production finished : $goodruns"
echo "Runs which are not ready : $badruns" >>& $logfile

#


foreach runlist ($runlistfiles)
  set filelist = `cat $runlist`

  foreach datafile ($filelist)
    set runnum = `basename $datafile .root | awk -F'-' '{printf "%d", $2}'`
#    echo "$datafile $runnum" >>& $logfile
    if( ! -f runlist/$runnum ) then
      #echo "D " $datafile $runnum

       set isbadrun = 0
       foreach badrun ($badruns)
           if ($badrun == $runnum) then
               set isbadrun = 1
               break
           endif
       end

#	echo "isbadrun = $isbadrun"
       if ($isbadrun == 0) then
          echo "$datafile" >>&  $newrunlist
          echo "goodrun $datafile" >>& $logfile
       endif
      
    else
      #echo "A " $datafile $runnum
    endif
  end
end

######################
# make mergelist using 
set listdir = "mergelist"
if( -d $listdir) then
  echo "remove all listfile" >>& $logfile
  rm $listdir/*.list
endif 

set mergelogfile = $autojobdir/make_mergelist3.log
if (-f $mergelogfile) then
    rm $mergelogfile
endif

#echo "before ./make_mergelist3.csh $newrunlist $listdir" >>& $logfile
./make_mergelist3.csh $newrunlist $listdir >>& $mergelogfile

echo "after ./make_mergelist3.csh $newrunlist $listdir" >>& $logfile

#####################
# submit

set merge_list = `ls $autojobdir/$listdir/*`

#sako
set joblogfile = $autojobdir/jobsubmit.log
if (-f $joblogfile) then
    rm $joblogfile
endif

set submitscript = "jobsubmit.csh"

echo "start submit jobs" >>& $logfile
foreach mergefile ($merge_list)
  set runno = `basename $mergefile .list| awk -F'_' '{printf "%d", $3}'`
  set seqno = `basename $mergefile .list| awk -F'_' '{printf "%04d", $4}'`
  echo "start $submitscript $mergefile $runno $seqno" >>&$logfile

  $submitscript $mergefile $runno $seqno >>& $joblogfile
end

echo "End search_newrun.csh" >>& $logfile
