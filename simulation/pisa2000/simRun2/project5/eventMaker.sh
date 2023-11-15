#!/bin/csh
#
# PSUB -b phenix                # Use phenix bank
# PSUB -o /nfs/tmp2/$USER/simRun2/project5/oscar/test.log # set log file
# PSUB -eo                      # Direct stderr to stdout
# PSUB -s /bin/csh              # Use csh
# PSUB -me                      # Send mail at end
# 
#! /usr/local/bin/tcsh -f

echo "Hostname: `hostname`"
echo "OS: `uname -a`"
echo "Runing in $ENVIRONMENT Environment"
printenv SESSARGS

set SAVEDIR=`pwd`

if ( $ENVIRONMENT =~ INTERACTIVE ) then
 setenv SESSARGS $1
 echo "Running interactively with $SESSARGS"
 cd /nfs/tmp2/$USER/simRun2/project5/oscar
else
 echo "Running in batch with $SESSARGS"
 source /usr/gapps/phenix/setup/phenix_setup.csh
 cd /nfs/tmp2/$USER/simRun2/project5/oscar
endif

echo "Date: `date`"
cp /usr/gapps/phenix/cvs/simulation/pisa2000/simRun2/project5/$SESSARGS\.input .
/usr/gapps/phenix/cvs/simulation/pisa2000/simRun2/project5/eventMaker.pl $SESSARGS
echo "Date: `date`"

cd $SAVEDIR

exit

