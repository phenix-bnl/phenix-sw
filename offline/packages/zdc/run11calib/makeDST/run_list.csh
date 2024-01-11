#!/bin/csh

set runlist = run.list

foreach list (`cat $runlist|sort`)

echo "Running on $list"
./OneRun_PRDF2DST.csh $list

end

echo "Finished PRDF2DST"

