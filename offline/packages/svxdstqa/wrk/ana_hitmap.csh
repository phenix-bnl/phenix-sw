#!/usr/local/bin/tcsh -f

cd /common/s2/Subsystems/hachiya/source/svxdstqa/wrk

source setup.csh

set outdir = "merge_anaoutdir/"
set logdir = "merge_anaoutlogdir/"

#set list = (`cat mergehist.list`)
set list = (`cat $1`)

foreach file ($list)
 set logfile = "$logdir/`basename $file .root`.log"
 echo "$file $logfile"
root -b << EOFF >& $logfile
.x rootsetup.C
.L ana_hitmap.C+
ana_hitmap("$file", "$outdir");
.q
EOFF

end
