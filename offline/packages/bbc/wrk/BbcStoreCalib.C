void BbcStoreCalib(Int_t maxEvents=1, Int_t verbose=1) {


  //
  // Set up the PHOOL initialization
  //
  gROOT->Macro("phoolRecoInit.C");

  // Executing initialization and parameter macros
  gROOT->Macro("BbcCalibini.C");
  gROOT->Macro("BbcCalibpar.C");
  PHTimeStamp time   = PHTimeStamp(2000,5,1,0,0,0);
  BbcCalib* bbccalib = new BbcCalib();
  bbccalib->restore("BbcCalib");
  bbccalib->showParameters();

  bbccalib->getConfig()->store(time,"config"); 
//bbccalib->getPedestal()->store(time,"pedestal");
//bbccalib->getOverflow0()->store(time,"overflow0");
//bbccalib->getOverflow1()->store(time,"overflow1"); 
//bbccalib->getPmtGain()->store(time,"pmtgain"); 
//bbccalib->getSlewing0()->store(time,"slewpar0"); 
//bbccalib->getSlewing1()->store(time,"slewpar1"); 
//bbccalib->getAdcGain()->store(time,"adc"); 
//bbccalib->getTdcGain0()->store(time,"tdc0"); 
//bbccalib->getTdcGain1()->store(time,"tdc1"); 
}

