#!/usr/local/bin/tcsh -f
#
# Copy job script
#
#  Usage: copy.csh infile outdir
#
#

set RCP_KEYFILE  = /phenix/u/htorii/RCP_KEYFILE

@ MINIMUM_FILESIZE = 1000

###############################################################

if( $# != 2) then
    echo "usage : $0 infile outdir"
    exit
endif
set IN_FILENAME  = $1
set OUT_DIRNAME = $2
if( -r ${IN_FILENAME} ) then
	echo " ..... Input file name is "${IN_FILENAME}
else
	echo " ..... Can't find input file : "${IN_FILENAME}
	exit
endif

###############################################################

echo " ..... Copying " `basename ${IN_FILENAME}` " into " ${OUT_DIRNAME}
echo " -----------------------------------------------------------------"
df -k
echo " -----------------------------------------------------------------"
@ loop = 10
while( ${loop} > 0 )
    echo "       ....... Looping ..... " ${loop}
    while( -e ${RCP_KEYFILE} )
	echo "       .. sleeping 5 second....."
	sleep 5
    end
    touch ${RCP_KEYFILE}
    hostname >> ${RCP_KEYFILE}
    echo "       ... Start copying... "
    #rcp ${NFS_SERVER}:${DIR_DST}/${filename} ${DIR_WORK}
    #cp -f ${DIR_DST}/${filename} ${DIR_WORK}
    cp ${IN_FILENAME} ${OUT_DIRNAME}/
    rm -f ${RCP_KEYFILE}
    set filename = `basename ${IN_FILENAME}`
    @ fsize = `ls -la ${OUT_DIRNAME}/${filename} | awk '{print $5}'`
    echo "       ....... fsize = " ${fsize}
    @ loop = ${loop} - 1
    if( ${fsize} > ${MINIMUM_FILESIZE} ) then
	@ loop = 0
	echo "  ........... Completed to copy. "
    endif
end

###############################################################

