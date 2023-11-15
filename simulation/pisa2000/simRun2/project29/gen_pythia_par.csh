#!/usr/local/bin/tcsh -f
#
# Batch job script
#

###############################################################
#
if( $# != 4 ) then
    echo "usage : $0 inputfile outputfile random_seed numevent"
    exit
endif
set INPUT_FILE = $1
set OUTPUT_FILE = $2
set RANDOM_SEED  = $3
set NUM_EVENT  = $4
if( -r ${INPUT_FILE} ) then
	echo " gen_pythia_par.csh:: File name of src file is "${INPUT_FILE}
else
	echo " gen_pythia_par.csh:: Can't open the src file: "${INPUT_FILE}
	exit
endif
echo " gen_pythia_par.csh:: output file name : "${OUTPUT_FILE}
###############################################################
sed 's|@_RANDOMSEED_@|'${RANDOM_SEED}'|g;s|@_NUM_EVENT_@|'${NUM_EVENT}'|g;' < $1 > $2

###############################################################
