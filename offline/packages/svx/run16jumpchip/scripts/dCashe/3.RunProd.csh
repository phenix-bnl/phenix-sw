#!/usr/local/bin/tcsh -f


################################################
#            Check Input Parameters            #
################################################

if( $#argv != 14 ) then
  echo 
  echo " *** ERROR! *** "
  echo " Please check arguments as follow "
  echo " Argument order : [1.tmprunscript] [2.copyscript] [3.runmacro] [4.id] [5.runnumber] [6.segnumber] [7.inprdfname] [8.nevent] [9.buildversion] [10.date] [11.SRCDIR] [12.OUTDIR] [13.TMPSCRIPTDIR] [14.userid]"
  echo
  exit 0
endif


################################################
#            Take Input Parameters             #
################################################

set tmprunscript = $1
set copyscript   = $2
set runmacro     = $3
set id           = $4
set runnumber    = $5
set segnumber    = $6
set inprdfname   = $7
set nevent       = $8
set buildversion = $9
set date         = $10
set SRCDIR       = $11
set OUTDIR       = $12
set TMPSCRIPTDIR = $13
set userid       = $14


################################################
#               Setup Enviroment               #
################################################

setenv HOME /phenix/u/${userid}
setenv prompt 1
source /etc/csh.login
foreach i (/etc/profile.d/*.csh)
  source $i
end
source $HOME/.login


echo "SRCDIR : " 
echo $SRCDIR

source /opt/phenix/bin/phenix_setup.csh ${buildversion}
setenv ODBCINI /opt/phenix/etc/odbc.ini.mirror
setenv LD_LIBRARY_PATH ${SRCDIR}/install/lib:$LD_LIBRARY_PATH

echo "ODBCINI : "
echo $ODBCINI


################################################
#                 Setup Run Dir                #
################################################

if ( ! ($?_CONDOR_SCRATCH_DIR) ) then
  echo
  echo " *** ERROR! *** "
  echo " _CONDOR_SCRATCH_DIR is not defined. "
  echo
  exit 0
endif

set TMPRUNDIR  = "${_CONDOR_SCRATCH_DIR}/${userid}/production/${id}/${date}/${runnumber}_${segnumber}"

rm -rf ${TMPRUNDIR}

if ( ! -d ${TMPRUNDIR} ) then
  mkdir -p ${TMPRUNDIR}
endif

cd ${TMPRUNDIR}
echo 
echo " Notice *** cd ${TMPRUNDIR}"
echo


################################################
#             Copy Files to Run Dir            #
################################################

/afs/rhic.bnl.gov/phenix/software/calibration/data/LuxorLinker.pl -1 ${runnumber}

cp -a ${SRCDIR}/scripts/dCashe/${copyscript} .
cp -a ${SRCDIR}/macros/${runmacro} .
cp -a ${SRCDIR}/macros/rawdatacheck.C .
cp -a ${SRCDIR}/macros/IOManager.C .


chmod +x ${copyscript}
echo
echo " Notice *** ${inprdfname} will be transferred from dCache to ${TMPRUNDIR} "
echo
./${copyscript} ${inprdfname} ${TMPRUNDIR}


################################################
#                  Run Macro                   #
################################################

root -b -q ''${runmacro}'('${nevent}',"'"${inprdfname}"'")'

ll -thr


################################################
#     Copy Output and Backup to Local Dir      #
################################################

mv vtxpjump.root ${OUTDIR}/vtxpjump-${runnumber}_0${segnumber}.root


################################################
#            Remove Temporary Files            #
################################################

cd ${OUTDIR}
echo
echo " Notice *** cd ${OUTDIR}"
echo

rm ${TMPSCRIPTDIR}/${tmprunscript}
rm -rf ${TMPRUNDIR}

echo
echo " Notice *** Condor job was finished! "
echo
