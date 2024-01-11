#!/usr/local/bin/tcsh
#
# Check script
#
#  Usage: nbatch_diff.csh dstlistfile done_dstlistfile out_file
#
# ls *.root > info_done_tmp
# awk 'BEGIN{FS="-"}{ print "/phenix/data13/phnxreco/v03/dsts1/DST_v03_Stream01-"$2"-"$3 }' info_done_tmp | sed 's/.root/.proot/' > info_done
# diff info_data13_0 info_done | awk '/^</{print $2}'
#


if( $# != 3) then
    echo "usage : $0 dstlistfile done_dstlistfile out_file
    exit
endif
set info_file  = $1
set check_file = $2
set out_file = $3
if( -r ${info_file} ) then
	echo " File name of DST list is "${info_file}
else
	echo " Can't open the DST list file: "${info_file}
	exit
endif
if( -e ${out_file} ) then
	echo " ${out_file} is existing..... Deleted "
	\rm -f ${out_file}
endif
touch ${out_file}
###############################################################
#
@ num = `wc ${dstlistfile}`

while( $num > 0 )
    set file = `awk '{if(NR=='$num') print $1}' ${dstlistfile}`
    @ onoff = 0
    @ onoff = `awk '{if($1='$file') print 1 }' ${done_dstlistfile}`
    if( onoff == 0 ) then
	echo ${file} >> ${out_file} 
	echo ${file} was not processed
    endif
    @ num = $num - 1
end
#
###############################################################
#
