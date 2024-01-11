
gSystem->Load("libemcAnalyzer.so");  emcAnalyzer e;
e.setVerbose(0);
e.setDumpLaTeX(); // output latex
e.setSaveEps(); // save eps

// Output final combined spectra

for (int i=0;i<100;i+=10){ int j=i+10; e.average_pbsc_pbgl(i,j); }; > latexout.txt
e.average_pbsc_pbgl(60,80); >> latexout.txt
e.average_pbsc_pbgl(70,92); >> latexout.txt
e.average_pbsc_pbgl(80,92); >> latexout.txt
e.average_pbsc_pbgl(0,100); >> latexout.txt

// Output final R_AA

for (int i=0;i<100;i+=10){ int j=i+10; e.plot_RAA(i,j); }; > latexout_RAA.txt
e.plot_RAA(60,80); >> latexout_RAA.txt
e.plot_RAA(70,92); >> latexout_RAA.txt
e.plot_RAA(80,92); >> latexout_RAA.txt
e.plot_RAA(0,100); >> latexout_RAA.txt
