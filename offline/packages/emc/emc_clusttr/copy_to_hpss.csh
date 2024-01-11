#!/usr/local/bin/tcsh -f
#
# Copy job script
#
#  Usage: copy_to_hpss.csh infile outfile
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
if( -r ${IN_FILENAME} ) then
	echo " ..... Input file name is "${IN_FILENAME}
else
	echo " ..... Can't find input file : "${IN_FILENAME}
	exit
endif

###############################################################

echo " ..... Copying " `basename ${IN_FILENAME}` " into " ${OUT_DIRNAME}
while( -e ${RCP_KEYFILE} )
echo "       .. sleeping 5 second....."
    sleep 5
end
touch ${RCP_KEYFILE}
hostname >> ${RCP_KEYFILE}
echo "       ... Start copying... "
set IN_DIRNAME = `dirname ${IN_FILENAME}`
set FILENAME = `basename ${IN_FILENAME}`
echo "cd ${OUT_DIRNAME} \n lcd ${IN_DIRNAME} \n put ${FILENAME}" | rftp
rm -f ${RCP_KEYFILE}
echo "       ... End copying... "

###############################################################

