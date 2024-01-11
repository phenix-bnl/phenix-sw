#!/usr/local/bin/tcsh -f

########################################################################################

# input parameters

@ i = 0
set njob      = 28 
set nevent    = 0
#set runnumber = 450137
#set runnumber = 450134
#set runnumber = 449987
#set runnumber = 449983
#set runnumber = 449980
set runnumber = 449990

#########################################################################################
 
set tmpdir   = `pwd`/tmp
set fun4all  = Fun4JumpChip.C 

if( ! -d ${tmpdir} ) then
  mkdir -p ${tmpdir}
endif


set macrodir = `pwd`/../../macros

while ( $i < ${njob} )

  set jobno = `printf "%03d" $i`

  @ re    = $i % 7
  set dir   = `printf "%d" $re `
  # for now we are using /common/b?/. It should be different according to buffer box
  set infile = `printf "/common/b%d/calibdata/CALIBDATA_P00-0000%d-0%03d.PRDFF" $dir ${runnumber} $i`
  if ( -e $infile ) then  
  
  ./2.Submit_Condor.csh 3.RunProd.csh ${jobno} ${tmpdir} ${macrodir} ${infile} ${nevent} ${runnumber} ${fun4all}
  echo $infile
  endif
  

  @ i ++

end
