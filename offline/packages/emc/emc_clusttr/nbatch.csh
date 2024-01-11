#!/usr/local/bin/tcsh -f
#
# Batch job script
#
#  Usage: batch.csh filename(dstlist) num(start) num(stop)
#
#

#set DIR_WORK     = "/job_tmp/htorii"
set DIR_WORK     = "/home/htorii_tmp"

set DIR_NTUPLE   = "/phenix/data24/htorii/01udst/v080701"

set HPSS_DIR     = "/home/htorii/Run01V01"

###############################################################
#
if( $# != 3) then
    echo "usage : $0 dstlistfile start end"
    exit
endif
set INFO_FILE  = $1
@ START        = $2
@ END          = $3
if( -r ${INFO_FILE} ) then
	echo " File name of DST list is "${INFO_FILE}
else
	echo " Can't open the DST list file: "${INFO_FILE}
	exit
endif

set pushdsilent
pushd `dirname $0`
set DIR_MACRO     = `pwd`
popd
echo " DIR_MACRO = " ${DIR_MACRO}

mkdir -p ${DIR_WORK}
set DIR_WORK = "${DIR_WORK}/${$}"
echo " Creating work directory :" ${DIR_WORK}
rm -fR $DIR_WORK
mkdir $DIR_WORK
cp ${INFO_FILE} ${DIR_WORK}/
cd $DIR_WORK

@ num = $START
while( $num <= $END )
    set longfilename = `cat $INFO_FILE | awk '{if (NR=='$num') print $1}' `
    set longdirname = `dirname ${longfilename}`
    set filename = `basename ${longfilename}`
    setenv ACTUAL_INPUT0 ${longfilename}
    setenv runnum `/phenix/workarea/htorii/Myana_01ana/Parse-ACTUAL_INPUT0.pl | awk '{print $1}'`
    setenv seqnum `/phenix/workarea/htorii/Myana_01ana/Parse-ACTUAL_INPUT0.pl | awk '{print $2}'`
    set flog = ${DIR_WORK}/log_${runnum}_${seqnum}.txt
    alias printlog 'echo \!* |& tee -a ${flog}'
    printlog "*****************************************************"
    printlog "*** Batch start *** "`date`
    printlog "*****************************************************"
    printlog "file number is " $num
    printlog "Open file name is " ${filename}                         
    printlog "               runnum:seqnum is " ${runnum}":"${seqnum}
    #######################################
    printlog " Executing copy.csh"
    /phenix/u/htorii/local/photon/Calibset1/copy.csh ${longfilename} ${DIR_WORK} >>& ${flog}
    printlog " Creating DST ... "
    set dst_filename = "DST_v00-${runnum}-${seqnum}.root"
    /phenix/workarea/htorii/Myana_01ana/wrk/run.csh ${DIR_WORK}/${filename} ${DIR_WORK}/${dst_filename} >>& ${flog}
    printlog " Creating uDST .... "
    set udst_filename = "uDST_v00-${runnum}-${seqnum}.root"
    /phenix/workarea/htorii/Myana_01ana/udst/run.csh ${DIR_WORK}/${dst_filename} ${DIR_WORK}/${udst_filename} >>& ${flog}
    printlog " Creating hist .... "
    set hist_filename = "hist_v00-${runnum}-${seqnum}.root"
    /phenix/data24/htorii/Myana_01ana/tofcalib/run.csh ${DIR_WORK}/${udst_filename} ${DIR_WORK}/${hist_filename} >> ${flog}
    printlog " ............................. Finished processing "
    #######################################
    printlog " Start transfer into HPSS ... "
    /phenix/u/htorii/local/photon/Calibset1/copy_to_hpss.csh ${DIR_WORK}/${dst_filename} ${HPSS_DIR} >> ${flog}
    /phenix/u/htorii/local/photon/Calibset1/copy_to_hpss.csh ${DIR_WORK}/${udst_filename} ${HPSS_DIR} >> ${flog}
    /phenix/u/htorii/local/photon/Calibset1/copy_to_hpss.csh ${DIR_WORK}/${hist_filename} ${HPSS_DIR} >> ${flog}
    /phenix/u/htorii/local/photon/Calibset1/copy_to_hpss.csh ${flog} ${HPSS_DIR} >> ${flog}
    printlog "Copying "${num} ${udst_filename} ${hist_filename}
    printlog "                         into " ${DIR_NTUPLE}
    cp -f ${DIR_WORK}/${udst_filename} ${DIR_WORK}/${hist_filename} ${DIR_NTUPLE}
    printlog " Deleting " ${DIR_WORK}/${filename}
    rm -f ${DIR_WORK}/${filename}
    printlog " Deleting " ${DIR_WORK}/${dst_filename}
    rm -f ${DIR_WORK}/${dst_filename}
    printlog " Deleting " ${DIR_WORK}/${udst_filename}
    rm -f ${DIR_WORK}/${udst_filename}
    printlog " Deleting " ${DIR_WORK}/${hist_filename}
    rm -f ${DIR_WORK}/${hist_filename}
##    if( ${longdirname} == "/phenix/data24/htorii/carousel" ) then
    set tmpname = `echo ${longdirname} |grep carousel`
    if(  ${tmpname} == ${longdirname} ) then
	printlog " Deleting and logging " ${longfilename}
	rm -f ${longfilename}
	mkdir -p ${longdirname}/Finished
	touch ${longdirname}/Finished/${filename}
    endif
    printlog "*****************************************************"
    printlog "*** Batch finished *** "`date`
    printlog "*****************************************************"
    printlog ""
    printlog "Copying " ${flog}
    printlog "                         into " ${DIR_NTUPLE}
    cp -f ${flog} ${DIR_NTUPLE}
    @ num = $num + 1
end

printlog " Deleting work directory :" ${DIR_WORK}
rm -fR ${DIR_WORK}

################################################################################################
################################################################################################
################################################################################################

