#!/usr/local/bin/tcsh -f

if ( -f /opt/phenix/bin/phenix_setup.csh ) then
  source /opt/phenix/bin/phenix_setup.csh
endif

set path = ( $path /opt/condor/bin )

set nonomatch # does not exit with no match error


set autojobdir = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/autoscript"
cd $autojobdir

set logfile = $autojobdir/"process_plots.log"
if (-f $logfile) then
    rm $logfile
endif

echo "start process_plots.csh" >> $logfile
touch "process_plots.run0"


set figdir = "/common/s2/Subsystems/hachiya/source/svxdstqa/wrk/merge_anaoutdir"
set largewebdir = "/home/phnxvtx/public_html/svxpixel/plot_qa_2d"
set smallwebdir = "/home/phnxvtx/public_html/svxpixel/plot_qa_2ds"

set runlist = `ls $autojobdir/runlist/??????`

foreach run ($runlist)
    set run = `basename $run`
#    echo $run

    touch "process_plots.run1"
    set largefigscom = `printf $figdir/hitmap_pixel_%6d_\*.png $run`
#echo $largefigscom
#set largefigs = `ls $figdir/hitmap_pixel_$run\_*.png`
    set largefigs = `ls $largefigscom`

    foreach largefig ($largefigs)
    
        set seq = `basename $largefig .png | awk -F'_' '{printf "%04d", $4}'`
	set smallfig = `printf $figdir/hitmaps_pixel_%6d_$seq.png $run`
	if (!(-f $smallfig)) then
	   set convcmd = "convert -geometry 100x80 $largefig $smallfig"
	   echo $convcmd >> $logfile
	   $convcmd
	endif

	set largelink = `printf $largewebdir/hitmap_pixel_%6d_$seq.png $run`
	if (!(-f $largelink)) then
	   set llinkcmd = "ln -s $largefig $largewebdir"
	   echo $llinkcmd >> $logfile
	   $llinkcmd
	endif

	set smalllink = `printf $smallwebdir/hitmaps_pixel_%6d_$seq.png $run`
	if (!(-f $smalllink)) then
	   echo $smalllink
	   set slinkcmd = "ln -s $smallfig $smallwebdir"
	   echo $slinkcmd >> $logfile
	   $slinkcmd
	endif

	set hotfile = `printf $figdir/SvxDstQA_Merge_%6d_%s_hot.txt $run $seq`
	set deadfile = `printf $figdir/SvxDstQA_Merge_%6d_%s_dead.txt $run $seq`
	set ratefile = `printf $figdir/SvxDstQA_Merge_%6d_%s_rate.txt $run $seq`

#	echo $hotfile >> $logfile

	set gziphotfile = `printf $figdir/SvxDstQA_Merge_%6d_%s_hot.txt.gz $run $seq`
	set gzipdeadfile = `printf $figdir/SvxDstQA_Merge_%6d_%s_dead.txt.gz $run $seq`
	set gzipratefile = `printf $figdir/SvxDstQA_Merge_%6d_%s_rate.txt.gz $run $seq`

	if (-f $hotfile) then
	    if (-f $gziphotfile) then
		rm $gziphotfile
		echo "rm $gziphotfile" >> $logfile
	    endif
	    /bin/gzip $hotfile
	    echo "/bin/gzip $hotfile" >> $logfile
	endif


	if (-f $deadfile) then
	    if (-f $gzipdeadfile) then
		rm $gzipdeadfile
		echo "rm $gzipdeadfile" >> $logfile
	    endif
	    /bin/gzip $deadfile
	    echo "/bin/gzip $deadfile" >> $logfile
	endif


	if (-f $ratefile) then
	    if (-f $gzipratefile) then
		rm $gzipratefile
		echo "rm $gzipratefile" >> $logfile
	    endif
	    /bin/gzip $ratefile
	    echo "/bin/gzip $ratefile" >> $logfile
	endif

    end


end



#set gziphotfile = "$outdir/`basename $file .root`_hot.txt.gz"
#set gzipdeadfile = "$outdir/`basename $file .root`_dead.txt.gz"
#set gzipratefile = "$outdir/`basename $file .root`_rate.txt.gz"
#
#
#set hotfilecmd = "gzip $outdir/`basename $file .root`_hot.txt"
#set deadfilecmd = "gzip $outdir/`basename $file .root`_dead.txt"
#set ratefilecmd = "gzip $outdir/`basename $file .root`_rate.txt"
#
#echo "$hotfilecmd"
#$hotfilecmd
#
#echo "$deadfilecmd"
#$deadfilecmd
#
#echo "$ratefilecmd"
#$ratefilecmd


touch "process_plots.run2"

echo "end process_plots.csh" >> $logfile

