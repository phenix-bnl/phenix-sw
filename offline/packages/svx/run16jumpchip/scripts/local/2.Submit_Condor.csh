#!/usr/local/bin/tcsh -f

source /opt/phenix/bin/phenix_setup.csh new

##########################
# CHECK INPUT PARAMETERS #
##########################

if ( $#argv != 8 ) then
  echo "Usage   : jobsubmit.csh [script] [jobno] [tmpdir] [macrodir] [inputfile] [nevent] [runnumber] [fun4all]"
  exit 0
endif

if( ! -f $1 ) then
  echo "File $1 is not ordinary"
  echo "No mergelist : $1"
  exit 0
endif

###################


set submitdir   = `pwd`
#set jobbasedir  = "$submitdir"
set script      = $1
set jobno       = $2
set tmpdir      = $3
set macrodir    = $4
set inputfile   = $5
set nevent      = $6
set runnumber   = $7
set fun4all     = $8

set arglist = ( ${jobno} ${tmpdir} ${macrodir} ${inputfile} ${nevent} ${runnumber} ${fun4all} )

unsetenv ONCAL_MAIN
unsetenv ONCAL_OUTDIR
unsetenv ONCAL_HTMLDIR

setenv ODBCINI /opt/phenix/etc/odbc.ini.master
setenv DCACHE_DOOR phnxdoor1.rcf.bnl.gov:22133

####################
# MAKING JOB FILES # 
####################

set date = `date +%Y%m%d`
set jobdir = "log/${runnumber}.${date}"
if( ! -d $jobdir ) then
  mkdir -p $jobdir
endif

set jobfile = "$jobdir/${jobno}.job"
set outfile = "$jobdir/${jobno}.out"
set errfile = "$jobdir/${jobno}.err"
set logfile = "$jobdir/${jobno}.log"

#################
# SUBMIT CONDOR #
#################

echo "Universe        = vanilla"                   >  $jobfile
echo "Executable      = $script"                   >> $jobfile
echo "Arguments       = "\"$arglist\"              >> $jobfile
echo "GetEnv          = False"                     >> $jobfile
echo "+Job_Type       = "\"Production\"            >> $jobfile
#echo "+Job_Type       = "\"cas\"                   >> $jobfile
echo "+Experiment     = "\"phenix\"               >> $jobfile
echo "Initialdir      = $submitdir"                >> $jobfile
#echo "Requirements    = TotalDisk > 20000000"      >> $jobfile
echo "Output          = $outfile"                  >> $jobfile
echo "Error           = $errfile"                  >> $jobfile
echo "Log             = $logfile"                  >> $jobfile
#echo "Priority        = 9950"                      >> $jobfile
echo "Queue"                                       >> $jobfile

/opt/condor/bin/condor_submit $jobfile
