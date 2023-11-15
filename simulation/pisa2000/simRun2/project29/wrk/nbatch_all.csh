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

if( $# != 2 ) then
    echo "usage : $0 start end"
    exit
endif
@ START        = $1
@ END          = $2
###############################################################
#

@ num = $START
while( $num <= $END )
    echo -n " Create batch :"$num" ? :(y,n) "
    set ans = $<
    if( $ans == 'n' ) then
	echo "     ...................Skiped... "
    else
	#######################################
	@ rand1 = `date +%s`
	@ rand2 = ${rand1} % 100
	@ rand3 = ( $rand2 + $rand1 ) % 10
	echo bsub -r -q ${QUEUE_NAME} -o job.%J.log -e job.%J.err /phenix/data12/htorii/Myana_01sim/project29/run.csh 1000 run${num} ${num} ${rand2}
	bsub -r -q ${QUEUE_NAME} -o job.%J.log -e job.%J.err /phenix/data12/htorii/Myana_01sim/project29/run.csh 1000 run${num} ${num} ${rand2}
	#######################################
    endif
    @ num = $num + 1
end

#
