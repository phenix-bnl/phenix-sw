#!/usr/local/bin/tcsh -f

#set autojobdir = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/autoscript"
#set mainlogfile = $autojobdir/make_mergelist3.log
#echo "In make_mergelist3.csh" >>& $mainlogfile

source /opt/phenix/bin/phenix_setup.csh
#########################
## argument check
if($#argv<2) then
  echo "Usage: make_mergelist3.csh [listfile] [outdir]"
  exit 0
endif

if( ! -f $1) then
  echo "no file : $1"
  exit 0
endif

if( ! -d $2) then
  echo "making directory : $2"
  mkdir $2
endif

#########################

set list=`cat $1`
set listdir = "$2"
set logfile = "log.list"


#########################
@ i = 0
@ runbase = 0
@ seqbase = 0


## new listfile condition
## change runnumber 
## sequence # is 30 far from previous one.
## one list should contain less than 10 files

foreach file ($list)
  set run = `basename $file .root| awk -F'-' '{printf "%d", $2}'`
  set seq = `basename $file .root| awk -F'-' '{printf "%d", $3}'`

echo "$run $seq"

@ seqdiff = $seq - $seqbase

#  echo "aa" $file $run $seq $seqdiff $i

  if( $run != $runbase || $seqdiff > 30 || $i>10 ) then
    set logfile = `printf "%s/SvxDstQA_Merge_%06d_%04d.list" $listdir $run $seq`
    echo $logfile $i
    echo "$logfile $i"

@ i = 0
@ runbase = $run

  endif

  echo $file $run $seq
  echo "$file $run $seq"
  echo "$file" >> $logfile


@ seqbase = $seq
@ i ++

end

echo "Bottom of make_mergelist3.csh"
