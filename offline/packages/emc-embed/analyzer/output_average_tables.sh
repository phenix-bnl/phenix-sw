#! /bin/csh
set verbose
# Usage: output_average_tables cut
#

set nargs = $#argv

if !( $nargs == 1 ) then
  echo '<I> Usage: source output_average_tables cut  , where: '
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

##### 0-10%, 10-20%, ..., 60-80% and 70-92% #####

if (${CENT1} == 60) @ CENT2 = 80
if (${CENT1} == 70) @ CENT2 = 92

if (${CENT1} == 80) @ CENT2 = 92
if (${CENT1} == 90) @ CENT2 = 100
if (${CENT2} == 100) @ CENT1 = 0

echo '<I> Producing average table for cut: ' ${CUT} ' and centrality ' ${CENT1} '-' ${CENT2} '%'

#  root -b << EOF  
  root << EOF  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  e.setVerbose(0);
  e.setDumpAscii();
  //e.setSaveEps();
  e.average_pbsc_pbgl(${CENT1},${CENT2},"${CUT}"); > ${DATAPATH}/pi0_emcal_${CENT1}_${CENT2}_${CUT}.txt
  //e.plot_comparison_pbsc_pbgl(${CENT1},${CENT2},"${CUT}");
  //e.plot_ratio_pbsc_pbgl(${CENT1},${CENT2},"${CUT}");
EOF

@ CENT1 += 10
@ CENT2 += 10
end

##### 60-70% and 70-80% #####

set CENT1 = 60
set CENT2 = 70

while (${CENT2}<81)

echo "<I> Producing average table for cut: " ${CUT} " and centrality " ${CENT1} "-" ${CENT2}

#  root -b<< EOF  
  root << EOF  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  e.setVerbose(0);
  e.setDumpAscii();
  //e.setSaveEps();
  e.average_pbsc_pbgl(${CENT1},${CENT2},"${CUT}"); > ${DATAPATH}/pi0_emcal_${CENT1}_${CENT2}_${CUT}.txt
  //e.plot_comparison_pbsc_pbgl(${CENT1},${CENT2},"${CUT}");
  //e.plot_ratio_pbsc_pbgl(${CENT1},${CENT2},"${CUT}");

EOF

@ CENT1 += 10
@ CENT2 += 10
end

##### 60-92% #####

set CENT1 = 60
set CENT2 = 92

echo "<I> Producing average table for cut: " ${CUT} " and centrality " ${CENT1} "-" ${CENT2}

  root << EOF  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  e.setVerbose(0);
  e.setDumpAscii();
  //e.setSaveEps();
  e.average_pbsc_pbgl(${CENT1},${CENT2},"${CUT}"); > ${DATAPATH}/pi0_emcal_${CENT1}_${CENT2}_${CUT}.txt
  //e.plot_comparison_pbsc_pbgl(${CENT1},${CENT2},"${CUT}");
  //e.plot_ratio_pbsc_pbgl(${CENT1},${CENT2},"${CUT}");

EOF

exit

########################

source output_average_tables.sh tof1chisq1
