#!/bin/csh 

if ( $# != 1 ) then
    echo "OneRun_PRDF2DST.csh [location of run]"
    exit
endif

set list = $argv[1]
set macro =  Fun4Zdc.C
set output = DSTOut
#set nevent = 10
set nevent = 100000

echo $macro\(${nevent},\"$list\",\"$output\"\)
root -q -l $macro\(${nevent},\"$list\",\"$output\"\)



