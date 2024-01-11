void BbcReStoreCalib(Int_t maxEvents=1, Int_t verbose=1) {


  //
  // Set up the PHOOL initialization
  //
  gROOT->Macro("phoolRecoInit.C");

  // Executing initialization and parameter macros
  gROOT->Macro("BbcCalibini.C");
  gROOT->Macro("BbcCalibpar.C");
  PHTimeStamp time   = PHTimeStamp(2001,6,31,0,0,0);
  BbcCalib* bbccalib = new BbcCalib();
  bbccalib->restore(time);
  bbccalib->showParameters();

}

