#! /bin/csh

set CENT1 = 0
set CENT2 = 5
 
while (${CENT1}<100)

  root -b<< EOF 
  gSystem->Load("pi0_spectra_run2_C.so");
  plot_pi0_spectra_run2(${CENT1},${CENT2},"full","no"); > tables0/pi0_pbsc_${CENT1}_${CENT2}.txt
EOF

@ CENT1 += 5
@ CENT2 += 5
end

set CENT1 = 0
set CENT2 = 10
 
while (${CENT1}<100)

  root -b<< EOF 
  gSystem->Load("pi0_spectra_run2_C.so");
  plot_pi0_spectra_run2(${CENT1},${CENT2},"full","no"); > tables0/pi0_pbsc_${CENT1}_${CENT2}.txt
EOF

@ CENT1 += 10
@ CENT2 += 10
end

root -b<< EOF 
 gSystem->Load("pi0_spectra_run2_C.so");
 plot_pi0_spectra_run2(80,92,"full","no"); > tables0/pi0_pbsc_80_92.txt
 plot_pi0_spectra_run2(100,100,"full","no"); > tables0/pi0_pbsc_minbias.txt
 combine_pi0_spectra_run2(60,80,"no"); > tables0/pi0_pbsc_60_80.txt
 combine_pi0_spectra_run2(60,92,"no"); > tables0/pi0_pbsc_60_92.txt
EOF
