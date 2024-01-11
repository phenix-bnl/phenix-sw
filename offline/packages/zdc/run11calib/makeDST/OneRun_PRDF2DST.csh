#!/bin/csh 

if ( $# != 1 ) then
    echo "OneRun_PRDF2DST.csh [location of run]"
    exit
endif

set list = $argv[1]
set macro =  Fun4Zdc.C
set output = ./
set nevent = 100000

root -q -l $macro\(${nevent},\"$list\",\"$output\"\)



