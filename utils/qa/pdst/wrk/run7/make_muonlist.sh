#!/bin/bash
OUTPUT_FILE=muonqalist.txt
DISK_LOC="/phenix/hl/lindenle/run7/qaRoot_muon/"
rm ${OUTPUT_FILE}
for FILE in $(find ${DISK_LOC}/ -name 'qaRoot_muon*.root'); do 

    FILENAME=$(basename ${FILE})
    RUN=$( echo $FILENAME | sed  -e s/.*pro78-// |  sed  -e s/_.*//)
    RUN=$(echo "$RUN + 0"  | bc -l)
    # here I will check to see that there is not already an entry in QA DB
    EXISTS=$(psql -U phnxrc -h phnxdb2.phenix.bnl.gov calibrations -q -t -o /dev/stdout -c "select count(distinct tag) from mutrqa where parname like '%Cath.Cluster Width%' and runnumber=$RUN";)
    EXISTS=$(echo "$EXISTS + 0" | bc -l)

    if [ "$EXISTS" -eq "0" ]; then
	echo "Adding $RUN to the list"
	echo "$RUN $FILE" >> ${OUTPUT_FILE}
    else
	echo "Not adding $RUN b/c we have MUTR QA DB entry"
    fi

done
exit
