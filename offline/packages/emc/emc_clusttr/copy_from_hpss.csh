#!/usr/local/bin/tcsh -f
#
# Copy job script
#
#  Usage: copy_from_hpss.csh infile outdir
#
#

set HPSS_GROUP = "phnxemc"
source /opt/phenix/bin/phenix_setup.csh
@ MAX_JOB = 30
@ SLEEP_TIME = 1
@ DEBUG = 1
@ CAL_SKIP = 0

############################################################################################

if( $# != 2) then
    echo "usage : $0 infile outfile"
    exit
endif
set IN_FILENAME  = $1
set OUT_DIRNAME = $2
if( -d ${OUT_DIRNAME} ) then
	if(${DEBUG}) echo " debug: Output directory name is "${OUT_DIRNAME}
else
	if(${DEBUG}) echo " debug: Can't find output directory : "${OUT_DIRNAME}
	exit
endif

############################################################################################
if(${DEBUG}) echo " debug: Copying " `basename ${IN_FILENAME}` "@HPSS into " ${OUT_DIRNAME}
@ loop = 0
while( ${loop} == 0  )
    @ cnum = `\mysql -hsql.phenix.bnl.gov -uphenix_c_user -pBrass_Ring phenix_carousel -e 'select * from Entries where entrydate>20011118120000 and user="'${USER}'" order by id;' | awk 'BEGIN{i=0}{if($9=="0")i++;}END{print i}'`
    if( ${cnum} < ${MAX_JOB} ) then
	@ loop = 1
    else
	if(${DEBUG}) echo " debug: current carousel jobs# = " ${cnum} " is larger than maximum job@ " ${MAX_JOB}
	sleep ${SLEEP_TIME}
    endif
end
if(${DEBUG}) echo " debug: current carousel jobs# = " ${cnum} " is smaller than maximum job@ " ${MAX_JOB}

set caltime = `date +%Y%m%d%H%M%S`
if(${DEBUG}) echo " debug: data carousel submit time = " ${caltime}

set OUT_HOSTNAME = ` df ${OUT_DIRNAME} | awk -F':' '{if(FNR==2&&NF==2) print $1}' `
if( ${OUT_HOSTNAME} == "" ) then
    set OUT_HOSTNAME = `hostname`
endif
if(${DEBUG}) echo " debug: out hostname = " ${OUT_HOSTNAME}

#if(${DEBUG}) echo " debug: Executing : " hpss_user.pl -g ${HPSS_GROUP} -r // ${OUT_HOSTNAME}/${OUT_DIRNAME} ${IN_FILENAME}
#hpss_user.pl -g ${HPSS_GROUP} -r // ${OUT_HOSTNAME}/${OUT_DIRNAME} ${IN_FILENAME} | tee tmp.txt

if(${DEBUG}) echo " debug: Executing : " hpss_user.pl -a -g ${HPSS_GROUP} -r // ${OUT_HOSTNAME}/${OUT_DIRNAME}/ ${IN_FILENAME}
set TMP_FILE = /tmp/htorii_hpss_$$
if( ${CAL_SKIP} ) then
    echo " Skiped because skip option" > ${TMP_FILE}
    echo " 1 records entered in DataBase " >> ${TMP_FILE}
else
    hpss_user.pl -a -g ${HPSS_GROUP} -r //${OUT_HOSTNAME}/${OUT_DIRNAME}/ ${IN_FILENAME} >& ${TMP_FILE}
endif
@ num = ` grep records ${TMP_FILE} | awk '{print $1}' `
echo " debug: ----------------------------------------------------- "
awk '{print " debug: " $0}' ${TMP_FILE}
echo " debug: ----------------------------------------------------- "
\rm -f ${TMP_FILE}
if( ${num} != 1 ) then
    echo " Error: hpss_user.pl failed . "
    exit
endif

############################################################################################

set FTPLOG_FILE = "/phenix/u/htorii/.carousel/Ftp_after.txt"
set filename = `basename ${IN_FILENAME}`
#set caltime =  20011120172922
if(${DEBUG}) echo -n " debug: "

@ loop = 0
while( ${loop} == 0 )
    if(${DEBUG}) grep ${OUT_DIRNAME} ${FTPLOG_FILE} |grep ${filename} | awk '{if($1>'${caltime}') print " debug: "$1 }'
    set ftptime = ` grep ${OUT_DIRNAME} ${FTPLOG_FILE} |grep ${filename} | awk '{if($1>'${caltime}') print $1 }' `
    if(${DEBUG} && ${ftptime} != "" ) echo " debug: ftptime is " ${ftptime}
    if(${DEBUG} && ${ftptime} == "" ) echo -n "."
    if( ${ftptime} > ${caltime} ) then
	@ loop = 1
    else
	sleep ${SLEEP_TIME}
    endif
end

if(${DEBUG}) echo " debug: End copying... "

###############################################################
###############################################################
#
# Backup for developper
#
#echo hpss_user.pl -g ${HPSS_GROUP} -r // ${SAVE_DIR} ${longfilename}
#hpss_user.pl -k -g ${HPSS_GROUP} -r // ${SAVE_DIR} ${longfilename}
#echo hpss_user.pl -upd -g ${HPSS_GROUP} ${longfilename} ${SAVE_DIR}/${filename}
#hpss_user.pl -upd -g ${HPSS_GROUP} ${longfilename} ${SAVE_DIR}/${filename}
###############################################################
