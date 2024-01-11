#! /bin/csh

set CENT1 = 0
set CENT2 = 5
 
while (${CENT1}<100)
  root -b<< EOF 
  gSystem->Load("pi0_spectra_run2_C.so");
  plot_ratios_centralities_pp(${CENT1},${CENT2},"no"); > tables/R_AA_${CENT1}_${CENT2}.txt
EOF

@ CENT1 += 5
@ CENT2 += 5
end

set CENT1 = 0
set CENT2 = 10
 
while (${CENT1}<100)
  root -b<< EOF 
  gSystem->Load("pi0_spectra_run2_C.so");
  plot_ratios_centralities_pp(${CENT1},${CENT2},"no"); > tables/R_AA_${CENT1}_${CENT2}.txt
EOF

@ CENT1 += 10
@ CENT2 += 10
end

root -b<< EOF 
 gSystem->Load("pi0_spectra_run2_C.so");
 plot_ratios_centralities_pp(60,80,"no"); > tables/R_AA_60_80.txt
 plot_ratios_centralities_pp(80,92,"no"); > tables/R_AA_80_92.txt
 plot_ratios_centralities_pp(100,100,"no"); > tables/R_AA_minbias.txt
EOF
