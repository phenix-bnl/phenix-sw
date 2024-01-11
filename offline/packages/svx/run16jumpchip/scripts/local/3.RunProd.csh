#!/usr/local/bin/tcsh -f

setenv HOME /home/phnxvtx
setenv prompt 1
source /etc/csh.login
foreach i (/etc/profile.d/*.csh)
  source $i
end
source $HOME/.login

source /opt/phenix/bin/phenix_setup.csh new
setenv ODBCINI /opt/phenix/etc/odbc.ini.master
setenv DCACHE_DOOR phnxdoor1.rcf.bnl.gov:22133
#setenv LD_LIBRARY_PATH /common/s16/vtx/tmoon/wrk/offline/packages/svx/run16jumpchip/install/lib:$LD_LIBRARY_PATH
setenv LD_LIBRARY_PATH `pwd`/../../install/lib:$LD_LIBRARY_PATH


if( $#argv != 7) then
  echo "   jobno     = jobno"
  echo "   tmpdir    = tmpdir"
  echo "   macrodir  = macrodir"
  echo "   infile    = inputfile"
  echo "   nevent    = nevent"
  echo "   runnumber = runnumber"
  echo "   fun4all   = fun4all"
  exit -1
endif

set jobno     = $1
set tmpdir    = $2
set macrodir  = $3
set infile    = $4
set nevent    = $5
set runnumber = $6
set fun4all   = $7

set tmpWdir    = "${tmpdir}/${runnumber}_${jobno}"
set outputdir = "${tmpdir}/../out"

if ( ! -d ${tmpWdir} ) then
  mkdir -p ${tmpWdir}
endif

if ( ! -d ${outputdir} ) then
  mkdir -p ${outputdir}
endif

echo "cd ${tmpWdir}"
cd       ${tmpWdir}

/afs/rhic.bnl.gov/phenix/software/calibration/data/LuxorLinker.pl -1 ${runnumber}

cp -a ${macrodir}/${fun4all} .
cp -a ${macrodir}/rawdatacheck.C .
#cp -a ${macrodir}/OutputManager.C .
cp -a ${macrodir}/IOManager.C .
#cp -a ${macrodir}/TrigSelect.C .

root -b -q ''${fun4all}'('${nevent}',"'"$infile"'")'

ll -thr

mv vtxpjump.root ${outputdir}/vtxpjump-0000${runnumber}-0${jobno}.root

cd ${outputdir}
rm -rf ${tmpWdir}
