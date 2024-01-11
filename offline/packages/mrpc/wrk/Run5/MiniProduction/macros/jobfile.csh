#! /bin/csh -f

unsetenv OFFLINE_MAIN
source /opt/phenix/bin/phenix_setup.csh new
setenv LD_LOCAL /phenix/data20/chujo/MRPC/offline/new_build/install.1/lib

setenv LD_LIBRARY_PATH ${LD_LOCAL}:${LD_LIBRARY_PATH}
echo ${LD_LIBRARY_PATH}

echo "SEGMENT = " ${SEGMENT}
echo "" 

setenv RunNumber 151834
setenv INPUTDIR /phenix/data20/chujo/PRDFF/Run5_CuCu_200GeV
setenv INFILE   EVENTDATAxxx_P01-0000${RunNumber}-${SEGMENT}.PRDFF
setenv PRDFNAME ${INPUTDIR}/${INFILE}

echo ${PRDFNAME}
echo ''

/afs/rhic/phenix/software/calibration/data/LuxorLinker.pl -1 151834

#########
echo '(( Fun4All -CNT- ))'
root.exe -b <<EOF 
.x Fun4All_cnt.C(0);
.q
EOF

/afs/rhic/phenix/software/calibration/data/LuxorLinker.pl -c
rm -f all.root
rm -f allthree.root
rm -f AlwaysDeadCh_dAu_72096.dat
rm -f CentCal.root
rm -f dataset-run4
rm -f DchEfficiency_dAu.Real
rm -f fieldIntegral.dat20Jan2004
rm -f hemmick*.root
rm -f hist.root
rm -f mybbcCal.root
rm -f path.root
rm -f pisafile_9jul03_run3.dat.cZ
rm -f Sim3D03.root
rm -f Sim3D++.root
rm -f zdcDandMult.root

mv DST_CNT_MinBias.root ../root/DST_CNT_MinBias_${RunNumber}-${SEGMENT}.root
