#! /bin/csh
set verbose
# Usage: output_tables cut
#

set nargs = $#argv

if !( $nargs == 1 ) then
  echo '<I> Usage: source output_tables cut  , where: '
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

#set CENT1 = 0
#set CENT2 = 5

#while (${CENT1}<100)

#  root -b<< EOF 
#  gSystem->Load("libemcAnalyzer.so");
#  emcAnalyzer e;
#  e.setVerbose(0);
#  e.plot_spectrum(${CENT1},${CENT2},"${CUT}"); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}.txt
#EOF

#@ CENT1 += 5
#@ CENT2 += 5
#end

set CENT1 = 0
set CENT2 = 10
 
while (${CENT2}<103)

##### 60-80% and 70-92% #####

if (${CENT1} == 60) @ CENT2 = 80
if (${CENT1} == 70) @ CENT2 = 92

if (${CENT1} == 80) @ CENT2 = 92 
#then 
#setenv CUT 'noPID' # we prefer "noPID" than "tof1chisq1" for most peripheral bin
#@ CENT2 = 92
#endif

if (${CENT1} == 90) @ CENT2 = 100
if (${CENT2} == 100) @ CENT1 = 0

echo "<I> Producing table for cut: " ${CUT} " and centrality " ${CENT1} "-" ${CENT2}

  root -b<< EOF  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  e.setVerbose(0);
  //e.setSaveEps();
  // default is y-shifted spectra 
  e.plot_spectrum(${CENT1},${CENT2},"${CUT}"); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}.txt
  e.setSaveEps(0);
  //e.plot_spectrum(${CENT1},${CENT2},"${CUT}","full",0); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}_noshift.txt
  //e.plot_spectrum(${CENT1},${CENT2},"${CUT}","full",2); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}_ptshift.txt

  //e.setSaveEps();
  //e.plot_comparison_qm_new(${CENT1},${CENT2},"${CUT}");
  //e.plot_ratio_qm_new(${CENT1},${CENT2},"${CUT}");

EOF

@ CENT1 += 10
@ CENT2 += 10
end

##### 60-70% and 70-80% #####

set CENT1 = 60
set CENT2 = 70

while (${CENT2}<81)

echo "<I> Producing table for cut: " ${CUT} " and centrality " ${CENT1} "-" ${CENT2}

  root -b<< EOF  
  gSystem->Load("libemcAnalyzer.so"); 
  emcAnalyzer e;
  e.setVerbose(0);
  //e.setSaveEps();
  // default is y-shifted spectra 
  e.plot_spectrum(${CENT1},${CENT2},"${CUT}"); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}.txt
  //e.setSaveEps(0);
  //e.plot_spectrum(${CENT1},${CENT2},"${CUT}","full",0); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}_noshift.txt
  //e.plot_spectrum(${CENT1},${CENT2},"${CUT}","full",2); > ${DATAPATH}/pi0_pbsc_${CENT1}_${CENT2}_${CUT}_ptshift.txt

  //e.setSaveEps();
  //e.plot_comparison_qm_new(${CENT1},${CENT2},"${CUT}");
  //e.plot_ratio_qm_new(${CENT1},${CENT2},"${CUT}");

EOF

@ CENT1 += 10
@ CENT2 += 10
end

#root -b<< EOF 
# gSystem->Load("pi0_spectra_run2_C.so");
# plot_pi0_spectra_run2(80,92,"${CUT}","full","no"); > tables/pi0_pbsc_80_92$_{CUT}.txt
# plot_pi0_spectra_run2(100,100,"${CUT}","full","no"); > tables/pi0_pbsc_minbias_${CUT}.txt
# combine_pi0_spectra_run2(60,80,"no"); > tables/pi0_pbsc_60_80${CUT}.txt
# combine_pi0_spectra_run2(60,92,"no"); > tables/pi0_pbsc_60_92${CUT}.txt
#EOF

exit

########################

source output_tables.sh tof1chisq1
source output_tables.sh noPID

source output_tables.sh chisq1
source output_tables.sh chisq2
source output_tables.sh tof1
source output_tables.sh tof2
source output_tables.sh tof1chisq2
source output_tables.sh tof2chisq1
source output_tables.sh tof2chisq2


## for 60-92% special case:

  gSystem->Load("libemcAnalyzer.so"); emcAnalyzer e;
  e.setVerbose(0);
  e.setSaveEps();
  e.plot_spectrum(60,92,"tof1chisq1"); > pi0_pbsc_60_92_tof1chisq1.txt
  e.setSaveEps(0);
  e.plot_spectrum(60,92,"tof1chisq1","full",0); > pi0_pbsc_60_92_tof1chisq1_noshift.txt
  e.plot_spectrum(60,92,"tof1chisq1","full",2); > pi0_pbsc_60_92_tof1chisq1_ptshift.txt

  e.plot_spectrum(60,92,"noPID"); > pi0_pbsc_60_92_noPID.txt
  e.setSaveEps(0);
  e.plot_spectrum(60,92,"noPID","full",0); > pi0_pbsc_60_92_noPID_noshift.txt
  e.plot_spectrum(60,92,"noPID","full",2); > pi0_pbsc_60_92_noPID_ptshift.txt


######
source ../output_tables.sh r012
source ../output_tables.sh r345
source ../output_tables.sh r678

rm $DATAPATH/pi0_pbsc_*0_10*txt
rm $DATAPATH/pi0_pbsc_*60_8*txt
rm $DATAPATH/pi0_pbsc_*92*txt

mv $DATAPATH/pi0_pbsc_*txt $DATAPATH/pi0_pbsc_tables/
