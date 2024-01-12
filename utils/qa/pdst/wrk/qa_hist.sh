#!/usr/local/bin/tcsh -f
#
# Batch job script for creating QA histograms file, to use type:
# qa_hist.sh <input-dst-file> <output-qahistogram-file>
# it creates a /tmp/qa_histxxxx.sh script, executes and deletes it
#
source /opt/phenix/bin/phenix_setup.csh
setenv LD_LIBRARY_PATH /phenix/workarea/marzia/qa/brendan/lib:${LD_LIBRARY_PATH}

###############################################################
if( $# != 2 ) then
    echo "usage : $0 DST outfile"
    exit
endif
set IN_FILENAME  = $1
set OUT_FILENAME = $2
if( -r ${IN_FILENAME} ) then
	echo " Input file name is "${IN_FILENAME}
else
	echo " Can't find input file : "${IN_FILENAME}
	exit
endif

################################################################################
echo " qa_hist.csh::*** qa_hist.csh start *** "`date`
#######################################
setenv TMPFILE /tmp/qa_hist_$$.cc
rm -rf ${TMPFILE}
echo " qa_hist.csh:: Create temporary file " ${TMPFILE}
cat <<EOF > ${TMPFILE}
#include <stdio.h>
#include <TChain.h>
#include <TFile.h>
void qa_hist_$$(char* infile,char* outfile, int nevts=0)
{

  TFile *f = new TFile(outfile,"RECREATE");   // Open output file first!
  gSystem->Load("liblvl2.so");
  gSystem->Load("libdstqa.so");
  qaRun(infile,outfile);
}
//
EOF
chmod uog+wr ${TMPFILE}
cat ${TMPFILE}
#######################################
echo " qa_hist.csh::Processing ............................................"
echo root -b -q ${TMPFILE}\(\"${IN_FILENAME}\",\"${OUT_FILENAME}\"\)
root -b -q ${TMPFILE}\(\"${IN_FILENAME}\",\"${OUT_FILENAME}\"\)
#######################################
echo " qa_hist.csh::...................................................... Finished processing "
echo -n " qa_hist.csh::"
ls -la ${OUT_FILENAME}
echo " qa_hist.csh::Delete "${TMPFILE}
rm -f ${TMPFILE}
echo " qa_hist.csh::*****************************************************"
echo " qa_hist.csh::*** qa_hist.csh finished *** "`date`
echo " qa_hist.csh::*****************************************************"
################################################################################




