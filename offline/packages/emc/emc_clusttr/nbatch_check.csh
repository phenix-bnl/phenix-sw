#!/usr/local/bin/tcsh
#
# Check script
#
#  Usage: nbatch_check.csh dstlistfile checkfile start end step
#
#

if( $# != 5) then
    echo "usage : $0 dstlistfile checkfile start end step"
    exit
endif
set info_file  = $1
set check_file = $2
@ start = $3
@ end = $4
set out_file = "nbatch_check_out.txt"
@ step = $5
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

@ num = $start
while( $num <= $end )
    set size = `awk '{if($2=='$num') print $4}' $check_file`
    if(! $size > 0 ) then
	set filename = `awk '{if (NR=='$num') print $1}' ${info_file} `
	@ numend = $num
	echo    " ID $num($filename) is not completed ...... "
	echo $num $filename >> ${out_file}
    endif
    @ num = $num + 1
end
#
###############################################################
#
@ loop = 1
echo `wc -l ${out_file}| awk '{print $1}' `
@ end = `wc -l ${out_file} | awk '{print $1}' `
echo `awk '{if (NR=='$loop') print $1}' ${out_file}`
@ numstart = `awk '{if (NR=='$loop') print $1}' ${out_file}`
@ numend = $numstart
@ num_prev = $numstart
while( $loop <= $end )
    @ num = `awk '{if (NR=='$loop') print $1}' ${out_file} `
    echo -n " "$loop"("$num")"
    #echo ".... num,numstart,numend,num_prev " $num $numstart $numend $num_prev
    if( $num > $num_prev + 1 && $numend == $num_prev) then
	    @ numend = $num_prev
	    @ numstart = $num
    else if($num > $num_prev + 1||$num == $numstart + $step - 1||$loop == $end)then
	if( $num == $numstart + $step - 1) then
	    @ numend = $numstart + $step - 1
	else  if( $loop == $end ) then
	    @ numend = $num
	else  if( $num > $num_prev + 1 && $numend != $num_prev) then
	    @ numend = $num_prev
	endif

	echo ""
	echo -n " Create batch("$info_file") (min,max) = ("$numstart","$numend")? :(y,n) "
	set ans = $<
	if( $ans == 'n' ) then
	    echo "     ...................Skiped... "
	else
	    #######################################
	    echo bsub -r -q phenix_cas -o job.%J.log -e job.%J.err ./nbatch.csh scan_info $numstart $numend
	    bsub -r q-q phenix_cas -o job.%J.log -e job.%J.err ./nbatch.csh scan_info $numstart $numend
	    #######################################
	endif
	@ tmp = $loop + 1
	if( $num == $numstart + $step - 1) then
	    @ numstart = `awk '{if (NR=='$tmp') print $1}' ${out_file} `
	else
	    @ numstart = $num
	endif
    endif
    @ loop = $loop + 1
    @ num_prev = $num
end

##################################################################

