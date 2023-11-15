#!/usr/local/bin/tcsh -f
# Batch job script
#
#  Usage: run.csh eventnum output randomseed1 randomseed2
#

#set DIR_WORK     = "/home/"${USER}"_wrk"
set DIR_WORK     = "/home/sim_project29_wrk"
set pushdsilent
pushd `dirname $0`
set SRC_DIR      = `pwd`
popd

###############################################################
if( $# != 4 ) then
    echo "usage : $0 cfmgmc_file output randomseed1 randomseed2"
    exit
endif
set cfmgmc_input = $1
@ eventnum = ` wc ${cfmgmc_input} | awk '{print $1}' `
set outfullfile = $2
@ randomseed1 = $3
#@ randomseed2 = $randomseed1 + `date +%s`
#@ randomseed2 = $randomseed2 % 100000
@ randomseed2 = $4
###############################################################
set pushdsilent
mkdir -p ${DIR_WORK}
set DIR_WORK = "${DIR_WORK}/${$}"
echo " Creating work directory :" ${DIR_WORK}
rm -fR $DIR_WORK
mkdir $DIR_WORK
set flog = ${DIR_WORK}/log.txt
alias printlog 'echo \!* |& tee -a ${flog}'
printlog " Using random seed sets ("${randomseed1}","${randomseed2}")"

printlog " ====================================================== "
printlog " ====================================================== "
printlog " Starting at `date` "
printlog " ====================================================== "

setenv OFFLINE_MAIN /afs/rhic/phenix/PHENIX_LIB/sys/i386_linux24/new.1/
source /opt/phenix/bin/phenix_setup.csh
setenv OO_FD_BOOT $PHENIX_FD_BOOT 

printlog " Copying " ${cfmgmc_input} "into work dir"
cp ${cfmgmc_input} ${DIR_WORK}/cfmgmc.input
printlog " Start ${eventnum} events production "
pushd $DIR_WORK
####################################################################################

printlog " Executing pisaBLinker.csh.. "
${SRC_DIR}/pisaBLinker.csh  >>& ${flog}
cp -f ${SRC_DIR}/pisa_nohbag.kumac ./pisa.kumac
cp -f ${SRC_DIR}/event_v0.par ./event.par
printlog " Executing pisa........ "
printlog " where is pisa : " `where pisa`
pisa <<EOF >>& ${flog}
    0
    
    
    
    rndm ${randomseed1} ${randomseed2}
    cfm_mul
    pt ${eventnum}
EOF

printlog " Executing PISAtoDST.C ........ "
${SRC_DIR}/taskBLinker.csh  >>& ${flog}
cp ${SRC_DIR}/PISAtoDST_emcbbc.C ${DIR_WORK}
root -l -b -q PISAtoDST_emcbbc.C\(${eventnum}\)  >>& ${flog}

printlog " Executing simDSTtoNtuple.cc ........ "
cp ${SRC_DIR}/simDSTtoNtuple.cc ${DIR_WORK}
root -l -b -q simDSTtoNtuple.cc\(\"simDST.root\",\"ntuple.root\"\) >>& ${flog}

printlog " Executing ana_ntuple.cc ........ "
cp ${SRC_DIR}/wrk/ana_cc.so ${DIR_WORK}
cp ${SRC_DIR}/wrk/ana_ntuple.cc ${DIR_WORK}
root -l -b -q ana_ntuple.cc\(\"ntuple.root\",\"ana.root\"\) >>& ${flog}

printlog " Executing emc_pi0sim ........ "
/phenix/data12/htorii/Myana_01ppv01burn1pi/prod/PREFIX/bin/emc_pi0sim_CL_dEmcGeaClusterTrack.csh ntuple.root pi0sim.root >>& ${flog}

popd
printlog " Copying pythia.hist pythia.dat PISAEVENT.root simDST.root "
printlog "     into " ${outfullfile} "_*.* "
#cp ${DIR_WORK}/pythia.hist ${outfullfile}_pythia.hist
#cp ${DIR_WORK}/pythia.dat ${outfullfile}_pythia.dat
gzip ${DIR_WORK}/cfmgmc.input
cp ${DIR_WORK}/cfmgmc.input.gz ${outfullfile}_cfmgmc.txt.gz
#cp ${DIR_WORK}/PISAEvent.root ${outfullfile}_PISAEvent.root
#cp ${DIR_WORK}/simDST.root ${outfullfile}_simDST.root
cp ${DIR_WORK}/ntuple.root ${outfullfile}_ntuple.root
cp ${DIR_WORK}/ana.root ${outfullfile}_ana.root
#cp ${DIR_WORK}/pi0sim.root ${outfullfile}_pi0sim.root
printlog " ====================================================== "
printlog " ====================================================== "
printlog " Enging at `date` "
printlog " ====================================================== "
####################################################################################
printlog " All standard output is saved as file : " ${outfullfile}_log.txt
printlog " Deleting work directory :" ${DIR_WORK}
gzip ${flog}
cp ${flog}.gz ${outfullfile}_log.txt.gz
rm -fR ${DIR_WORK}

####################################################################################
