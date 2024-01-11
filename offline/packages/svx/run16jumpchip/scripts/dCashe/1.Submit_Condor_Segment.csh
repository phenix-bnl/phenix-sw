#!/usr/local/bin/tcsh -f 

################################################
#            Check Input Parameters            #
################################################

if( $#argv != 14 ) then
  echo
  echo " *** ERROR! *** "
  echo " Please check arguments as follow "
  echo " Argument order : [1.condorscript] [2.runscript] [3.copyscript] [4.runmacro] [5.id] [6.runnumber] [7.isegnumber] [8.fsegnumber] [9.nevent] [10.buildversion] [11.INPRDFDIR] [12.BASEOUTDIR] [13.DATATYPE] [14.userid]"
  echo
  exit 0
endif



################################################
#               Input parameters               #
################################################

set condorscript = $1
set runscript    = $2
set copyscript   = $3
set runmacro     = $4

set id           = $5
set runnumber    = $6
set isegnumber   = $7
set fsegnumber   = $8
set nevent       = $9

set buildversion = $10

set INPRDFDIR    = $11
set BASEOUTDIR   = $12
set DATATYPE     = $13
set userid       = $14


################################################

set BASEDIR       = `pwd`
set SRCDIR        = ${BASEDIR}/../..

################################################

set OUTDIR       = ${BASEOUTDIR}/out/prdfTodst/production/${id}

if ( ! -d ${OUTDIR} ) then
  mkdir -p ${OUTDIR}
endif

################################################


while ( ${isegnumber} <= ${fsegnumber} )

  set inprdfname = `printf "${DATATYPE}_P00-0000%d-0%03d.PRDFF" ${runnumber} ${isegnumber}`

  if ( -e ${INPRDFDIR}/${inprdfname} ) then


    ./${condorscript} ${runscript} ${copyscript} ${runmacro} ${id} ${runnumber} ${isegnumber} ${inprdfname} ${nevent} ${buildversion} ${BASEDIR} ${SRCDIR} ${OUTDIR} ${userid}

  endif

  @ isegnumber ++

end

################################################
