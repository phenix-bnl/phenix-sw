#!/usr/local/bin/tcsh -f

if($#argv<2) then
  echo "Usage : merge_and_analysis.csh [listfile] [runnum]"
  exit 0
endif

echo "listfile   = $1"
echo "runnum     = $2"

set wrkdir = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/"
set mergeoutdir  = "$wrkdir/merge_outdir"
set anaoutdir    = "$wrkdir/merge_anaoutdir"
set anaoutlogdir = "$wrkdir/merge_anaoutlogdir"
set endrundir    = "$wrkdir/autoscript/runlist"

#echo "before source phenix_setup.csh"
#
#if ( -f /opt/phenix/bin/phenix_setup.csh ) then
#  source /opt/phenix/bin/phenix_setup.csh
#endif
#
#echo "after source phenix_setup.csh"

cd $wrkdir
echo "pwd"
pwd

echo "before source setup.csh"
source $wrkdir/setup.csh
echo "after source setup.csh"

cd $wrkdir
echo "pwd"
pwd

set listfile   = $1
set runnum     = $2
set fileheader = `basename $listfile .list`
set mergefile  = $mergeoutdir/${fileheader}.root
#set logfile    = $anaoutlogdir/${fileheader}.log

echo $listfile
echo $mergefile
ls mergehist.C
ls ana_hitmap.C

###############
# make mergeroot file
#root -b <<EOFF >& $logfile
root.exe -b <<EOFF
.L mergehist.C+
mergehist("$listfile", "$mergefile");
.q
EOFF

###############
# run ana_hitmap
#root -b << EOFF >>& $logfile
root.exe -b << EOFF
.x rootsetup.C
.L ana_hitmap.C+
ana_hitmap("$mergefile", "$anaoutdir");
.q
EOFF

###############
# end process
echo "done" > "$endrundir/$runnum"

