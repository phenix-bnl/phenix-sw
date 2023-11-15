#!/usr/local/bin/tcsh -f
#
#
#
#

if( $# != 3 && $# != 6 ) then
    echo "usage : $0 particleid momentum(GeV/c) number [phi] [theta] [cone]"
    exit
endif

#Gamma = 1
#AntiProton = 15
#AntiNeutron 25

set pid = $1
set mom = $2
@ line = $3
set phi   = 0
set theta = 90
set cone  = 9
if( $# == 6 ) then
    set phi = $4
    set theta = $5
    set cone = $6
endif
@ num = 1
@ verx = 0
@ very = 0
@ verz = 0
@ dmom = 0
############################################################
while( $line > 0 )
    echo $pid $mom $theta $phi $cone $num $verx $very $verz $dmom
    @ line = $line - 1
end
############################################################
