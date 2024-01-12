#!/bin/bash
INPUT_FILE=hv_files.list
PSQL="psql calibrations -U phnxrc -h phnxdb2.phenix.bnl.gov -c"

#note you really need to change this tag for everything that you have
#already put in.

TAG="run7AuAu_Muon_200Gev_pro78_new"

while read file; do 
    NORTHDEAD=$(cat $file | grep '^N' | wc -l )
    SOUTHDEAD=$(cat $file | grep '^S' | wc -l )
    RUN=$(echo $file | sed -e s/.*\_run//)
    echo "$RUN S:$SOUTHDEAD N:$NORTHDEAD" 
    $PSQL "insert into mutrqa (runnumber,inserttime,tag,parname,parameter,parerror) values ($RUN,extract('epoch' from now()),'$TAG','North:  No. HV Disabled',${NORTHDEAD},0)"
#   echo "insert into mutrqa (runnumber,inserttime,tag,parname,parameter,parerror) values ($RUN,extract('epoch' from now()),'$TAG','North:  No. HV Disabled',${NORTHDEAD},0)"
    $PSQL "insert into mutrqa (runnumber,inserttime,tag,parname,parameter,parerror) values ($RUN,extract('epoch' from now()),'$TAG','South:  No. HV Disabled',${SOUTHDEAD},0)"
done < $INPUT_FILE
