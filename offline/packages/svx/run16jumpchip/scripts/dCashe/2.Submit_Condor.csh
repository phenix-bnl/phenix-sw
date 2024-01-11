#!/usr/local/bin/tcsh -f


################################################
#            Check Input Parameters            #
################################################

if ( $#argv != 13 ) then
  echo
  echo " *** ERROR! *** "
  echo "Usage   : ./2.Submit_Condor.csh [1.runscript] [2.copyscript] [3.runmacro] [4.id] [5.runnumber] [6.isegnumber] [7.inprdfname] [8.nevent] [9.buildversion] [10.BASEDIR] [11.SRCDIR] [12.OUTDIR] [13.userid]"
  echo
  exit 0
endif

if( ! -f $1 ) then
  echo
  echo " *** ERROR! *** "
  echo "File $1 is not ordinary"
  echo "No mergelist : $1"
  echo
  exit 0
endif


################################################
#            Take Input Parameters             #
################################################

set runscript    = $1
set copyscript   = $2
set runmacro     = $3
set id           = $4
set runnumber    = $5
set isegnumber   = $6
set inprdfname   = $7
set nevent       = $8
set buildversion = $9
set BASEDIR      = $10
set SRCDIR       = $11
set OUTDIR       = $12
set userid       = $13

################################################
#               Setup Enviroment               #
################################################

source /opt/phenix/bin/phenix_setup.csh ${buildversion} 
setenv ODBCINI /opt/phenix/etc/odbc.ini.mirror


################################################
#               Setup Jobfiles                 #
################################################
  
set segnumber = `printf "%03d" ${isegnumber}`

set date = `date +%Y%m%d`
set jobdir = "log/${id}.${date}"
if( ! -d ${jobdir} ) then
  mkdir -p ${jobdir}
endif

set jobfile = "${jobdir}/${runnumber}_${segnumber}.job"
set outfile = "${jobdir}/${runnumber}_${segnumber}.out"
set errfile = "${jobdir}/${runnumber}_${segnumber}.err"
set logfile = "${jobdir}/${runnumber}_${segnumber}.log"


################################################
#            Make Tmpdir of Script             #
################################################

set TMPSCRIPTDIR = ${OUTDIR}/tmpscript
set tmprunscript = ${runnumber}_${segnumber}.csh

if ( ! -d ${TMPSCRIPTDIR} ) then
  mkdir -p ${TMPSCRIPTDIR}
endif

cp -a ${runscript} ${TMPSCRIPTDIR}/${tmprunscript}


################################################
#            Hand over Parameters              #
################################################

set arglist = ( ${tmprunscript} ${copyscript} ${runmacro} ${id} ${runnumber} ${segnumber} ${inprdfname} ${nevent} ${buildversion} ${date} ${SRCDIR} ${OUTDIR} ${TMPSCRIPTDIR} ${userid} )


################################################
#               Submit Condor                  #
################################################

echo "Universe        = vanilla"                                                                  >  ${jobfile}
echo "Executable      = ${TMPSCRIPTDIR}/${tmprunscript}"                                          >> ${jobfile}
echo 'Arguments       = "'${arglist}'"'                                                           >> ${jobfile}
echo "GetEnv          = False"                                                                    >> ${jobfile}
#echo "+Job_Type       = "\"Production\"                                                           >> ${jobfile}
echo '+Job_Type       = "cas"'                                                                    >> ${jobfile}
echo '+Experiment     = "phenix"'                                                                 >> ${jobfile}
echo "Initialdir      = ${BASEDIR}"                                                               >> ${jobfile}
#condor not run with TotalDisk > 3000000000 (Dec 4th 2015)
echo 'Requirements    = (CPU_Speed >= 1 && TotalDisk > 2500000000 && CPU_Experiment == "phenix")' >> ${jobfile}
#echo "concurrency_limits = READNFS"                                                               >> ${jobfile}
echo "Output          = ${outfile}"                                                               >> ${jobfile}
echo "Error           = ${errfile}"                                                               >> ${jobfile}
echo "Log             = ${logfile}"                                                               >> ${jobfile}
#echo "Priority        = 9950"                                                                     >> ${jobfile}
echo "Queue"                                                                                      >> ${jobfile}

condor_submit ${jobfile}
