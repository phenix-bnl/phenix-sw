#! /bin/csh
set verbose
# Usage: output_average_tables cut
#

set nargs = $#argv

if !( $nargs == 1 ) then
  echo '<I> Usage: source output_RAA cut  , where: '
  echo 'cut = noPID'
  echo 'cut = chisq1'
  echo 'cut = chisq2'
  echo 'cut = tof1chisq1'
  echo 'cut = tof1chisq2'
  echo 'cut = tof1'
  echo 'cut = tof2chisq1'
  echo 'cut = tof2chisq2'
  echo 'cut = tof2'
  echo ' '
  exit
endif

setenv CUT $argv[1]

setenv DATAPATH $AFSHOME/offline/packages/emc-embed/analyzer/data

set CENT1 = 0
set CENT2 = 10
 
while (${CENT2}<103)

##### 60-80% and 70-92% #####

if (${CENT1} == 60) @ CENT2 = 80
if (${CENT1} == 70) @ CENT2 = 92

if (${CENT1} == 80) @ CENT2 = 92
if (${CENT1} == 90) @ CENT2 = 100
if (${CENT2} == 100) @ CENT1 = 0

echo '<I> Producing R_AA table for cut: ' ${CUT} ' and centrality ' ${CENT1} '-' ${CENT2} '%'

#  root -b << EOF  
  root<< EOF  
  gSystem->Load("libemcAnalyzer.so");
  emcAnalyzer e;
  e.setVerbose(0);
  e.setDumpAscii();
  e.plot_RAA(${CENT1},${CENT2});  > ${DATAPATH}/RAA_emcal_${CENT1}_${CENT2}_${CUT}.txt

EOF

@ CENT1 += 10
@ CENT2 += 10
end

##### 60-70% and 70-80% #####

set CENT1 = 60
set CENT2 = 70

while (${CENT2}<81)

echo "<I> Producing R_AA table for cut: " ${CUT} " and centrality " ${CENT1} "-" ${CENT2}

  root -b<< EOF  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  e.setVerbose(0);
  e.setDumpAscii();
  e.plot_RAA(${CENT1},${CENT2});  > ${DATAPATH}/RAA_emcal_${CENT1}_${CENT2}_${CUT}.txt

EOF

@ CENT1 += 10
@ CENT2 += 10
end

exit

source output_RAA.sh tof1chisq1
