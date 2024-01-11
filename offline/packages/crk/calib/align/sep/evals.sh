#! /bin/tcsh

set dir = $1
source /opt/phenix/bin/phenix_setup.csh

setenv OO_FD_BOOT $PHENIX_FD_BOOT

set i=0

set ndst=`wc dstlist.txt | awk '{print $1}'`

while ($i < $ndst)
    @ i++
    set fin=`head -$i dstlist.txt |tail -1 | awk '{print $1}'`
    set nevt=`head -$i dstlist.txt |tail -1 | awk '{print $2}'`
    set ievt=0
    rm -f dst.root
    ln -s $dir/$fin dst.root
    while ($ievt < $nevt)
	root -b -q \
	    run_align.C\(\"dst.root\",\"evals/$ievt\_$fin\",$ievt,1000\)
	@ ievt+=1000
    end
    rm -f dst.root
end

