void centcalibntp() {
                                                                                                                             
  gSystem->Load("libfun4all.so");
  gSystem->Load("/phenix/data23/lebedev/offline/packages/uti/install/lib/libuti.so");
                                                                                                                             
  CentralityCalibrator *calib = new CentralityCalibrator();
                                                                                                                             
  cout << "Analysis started ";
  gSystem->Exec("date");

  calib->CalibrateFromNtuple("centcalib20.root");

  cout << "Analysis finished ";
  gSystem->Exec("date");
                                                                                                                             
}
                                                                                                                             

