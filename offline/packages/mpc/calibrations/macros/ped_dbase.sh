#!/bin/bash
#
# Save a bunch of XXXXXX.ped files to the database
#
#   XXXXXX is the run-number
#
# These files can be generated using newped.C("DST_MPC.root"),
# where the root file is ..
#

if [[ $# -lt 1 ]]
then
  echo "usage: ped_dbase.sh <listfile>"
  exit 1
fi

listfile=$1

echo -n "Please Enter Your Name: "
read user
echo -n "Please Enter the Database Message: "
read mesg

cat $listfile | while read pedfile
do
  echo Saving $pedfile to database
  runnumber=${pedfile#MpcCal_}
  runnumber=${runnumber%.ped}
  echo mpcdbase -w -y -u "$user" -m "$mesg" -r $runnumber $pedfile
  mpcdbase -w -y -u "$user" -m "$mesg" -r $runnumber $pedfile
done
