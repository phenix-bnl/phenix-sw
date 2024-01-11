#!/usr/local/bin/tcsh -f 
#
# author : Taebong.Moon 
# email  : taebong.moon87@gmail.com
# data   : Mar 24th 2016
#
# if condor is always "I", check "TotalDisk" requirement in 2.Submit_Condor.csh 
# if build version is not new, it may not be ran : check "copy_prdf.pl" 
# run(s) will executed ( isegnumber <= segment number <= fsegnumber )
#

################################################
#               Input parameters               #
################################################

set userid       = tmoon # this is needed to set up environment 
set firstrun     = 447072 # runnumber you want to run
set isegnumber   = 0 # look at above 
set fsegnumber   = 1 # look at above
set nevent       = 100 # 0 is to run all events in PRDF
set whatrun      = run16 # eg. run15, run16 and so on
set whatdata     = 1 # 0 and 1 for zero-field run and physics run, respectively.
set BASEOUTDIR   = /gpfs/mnt/gpfs02/phenix/hhj/hhj3/tmoon/jumppixel_study # base directory for output files


################################################
#                Don't Touch -!!               #
################################################

set id           = ${firstrun}
set lastrun      = ${firstrun}
set buildversion = new

if      ( ${whatdata} == 0 ) then
  set EVENTDATADIR = /direct/phenix+pnfs/phnxsink/${whatrun}/zerofdata 
  set DATATYPE     = ZEROFDATA
else if ( ${whatdata} == 1 ) then
  set EVENTDATADIR = /direct/phenix+pnfs/phnxsink/${whatrun}/eventdata 
  set DATATYPE     = EVENTDATA
else
  echo "ERROR : Wrong parameter inputed!"
  exit 0
endif

################################################
#                Don't Touch -!!               #
################################################

set nextcondorscript = 1.Submit_Condor_Segment.csh 
set condorscript     = 2.Submit_Condor.csh
set runscript        = 3.RunProd.csh
set copyscript       = copy_prdf.pl
set runmacro         = Fun4JumpChip.C
set tmpdirlist       = tmp.txt


################################################

ls ${EVENTDATADIR} > ${tmpdirlist}

################################################

foreach PERIOD ( `cat ${tmpdirlist}` )


  set INPRDFDIR = ${EVENTDATADIR}/${PERIOD}

  foreach filename ( `ls ${INPRDFDIR}/${DATATYPE}_P00-0000??????-0000.PRDFF` ) # search if input file is in dCashe or not

    set runnumber = `basename $filename | awk '{printf substr($0,19,6)}'`

    if ( ${firstrun} <= ${runnumber} && ${runnumber} <= ${lastrun} ) then

      ./${nextcondorscript} ${condorscript} ${runscript} ${copyscript} ${runmacro} ${id} ${runnumber} ${isegnumber} ${fsegnumber} ${nevent} ${buildversion} ${INPRDFDIR} ${BASEOUTDIR} ${DATATYPE} ${userid}

    endif

  end # filename

end # PERIOD

################################################

rm ${tmpdirlist}

################################################
