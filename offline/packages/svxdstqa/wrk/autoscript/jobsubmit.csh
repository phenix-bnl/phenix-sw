#!/usr/local/bin/tcsh -f


if ( $#argv < 3) then
  echo "Usage : jobsubmit.csh [mergelist] [runnum] [seqnum]"
  exit 0
endif

if( ! -f $1 ) then
  echo "No mergelist : $1"
  exit 0
endif

###################

source /opt/phenix/bin/phenix_setup.csh
#set path = ( $path /opt/condor/bin )

set submitdir   = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/autoscript/"
set jobbasedir  = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/autoscript/jobs"
set script = "$submitdir/merge_and_analysis.csh"
set mergelist = $1
set runnum    = $2
set seqnum    = $3
set mergelisthead = `basename $mergelist .list`

##################
# make jobfile

set jobdir = "$jobbasedir/$runnum"
if( ! -d $jobdir ) then
  mkdir $jobdir
endif

set jobfile = "$jobdir/run_${runnum}_${seqnum}.job"
set outfile = "$jobdir/${mergelisthead}.out"
set errfile = "$jobdir/${mergelisthead}.err"
set logfile = "$jobdir/${mergelisthead}.log"

echo "Universe        = vanilla"                   >  $jobfile
echo "Executable      = $script"                   >> $jobfile
#echo 'Arguments       = \'"$mergelist $runnum"'"' >> $jobfile
echo "Arguments       = "\"$mergelist $runnum\"    >> $jobfile
echo "GetEnv          = False"                     >> $jobfile
#echo "+Job_Type       = "\"Production\"            >> $jobfile
echo "+Job_Type       = "\"phnxvtx\"               >> $jobfile
echo "Initialdir      = $submitdir"                >> $jobfile
echo "Requirements    = TotalDisk > 20000000"      >> $jobfile
echo "Output          = $outfile"                  >> $jobfile
echo "Error           = $errfile"                  >> $jobfile
echo "Log             = $logfile"                  >> $jobfile
echo "Priority        = 9950"                      >> $jobfile
echo "Queue"                                       >> $jobfile


/opt/condor/bin/condor_submit $jobfile

