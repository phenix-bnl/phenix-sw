#!/usr/local/bin/tcsh
#
# Batch job script
#
#  Usage: nbatch_all.csh filename(dstlist) num(start) num(stop) num(step)
#
#

#set QUEUE_NAME = "sim_short"
source /opt/phenix/bin/phenix_setup.csh
set QUEUE_NAME = "phenix_cas"

if( $# != 4) then
    echo "usage : $0 dstlistfile start end step"
    exit
endif
set INFO_FILE  = $1
@ START        = $2
@ END          = $3
@ STEP         = $4
if( -r ${INFO_FILE} ) then
	echo " File name of DST list is "${INFO_FILE}
else
	echo " Can't open the DST list file: "${INFO_FILE}
	exit
endif
###############################################################
#

@ num = $START
while( $num <= $END )
    @ numend = $num + $STEP - 1
    if( $numend > $END ) then
	@ numend = $END
    endif
    echo -n " Create batch("$INFO_FILE") (min,max) = ("$num","$numend")? :(y,n) "
    set ans = $<
    if( $ans == 'n' ) then
	echo "     ...................Skiped... "
    else
	#######################################
	echo bsub -r -q ${QUEUE_NAME} -o job.%J.log -e job.%J.err ./nbatch.csh ${INFO_FILE} $num $numend
	bsub -q ${QUEUE_NAME} -o job.%J.log -e job.%J.err ./nbatch.csh ${INFO_FILE} $num $numend
	#######################################
    endif
    @ num = $numend + 1
end

#
