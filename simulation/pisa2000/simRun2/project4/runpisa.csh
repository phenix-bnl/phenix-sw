#!/bin/csh
#
# PSUB -b phenix                # Use phenix bank
# PSUB -eo                      # Direct stderr to stdout
# PSUB -s /bin/csh              # Use csh
# PSUB -me                      # Send mail at end
# 

echo "Date: `date`"
echo "Hostname: `hostname`"
echo "OS: `uname -a`"
echo $SESSARGS

source /usr/gapps/phenix/setup/phenix_setup.csh

cd $SESSARGS

pisa < pisa.input >& pisa.out

chgrp phenix *
chmod g+rw *

echo "Date: `date`"

exit


