#!/usr/local/bin/tcsh -f
#
# Copy job script
#
#  Usage: copy_from_hpss.csh infile outfile
#
#

set RCP_KEYFILE  = /phenix/u/htorii/RCP_KEYFILE

###############################################################

if( $# != 2) then
    echo "usage : $0 infile outfile"
    exit
endif
set IN_FILENAME  = $1
set OUT_DIRNAME = $2

###############################################################

echo " ..... Copying "`basename ${IN_FILENAME}`"@HPSS into " ${OUT_DIRNAME}
while( -e ${RCP_KEYFILE} )
echo "       .. sleeping 5 second....."
    sleep 5
end
touch ${RCP_KEYFILE}
hostname >> ${RCP_KEYFILE}
echo "       ... Start copying... "
set IN_DIRNAME = `dirname ${IN_FILENAME}`
set FILENAME = `basename ${IN_FILENAME}`
echo "cd ${IN_DIRNAME} \n lcd ${OUT_DIRNAME} \n get ${FILENAME}" | rftp
rm -f ${RCP_KEYFILE}
echo "       ... End copying... "

###############################################################

