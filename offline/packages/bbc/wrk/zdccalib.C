{
  gROOT->Macro("phoolInit.C");
  // Loading subsystem libraries
  gSystem->Load("libzdc.so");

  PHTimeStamp tSearch = PHTimeStamp(2000,7,15,0,0,0);
  ZdcCalib* ZdcCalibPar = new ZdcCalib();
  ZdcCalibPar->restore(tSearch);

  ZdcCalibPar->showParameters();

  ZdcCalibPar<PdbPmtPeak>   *Pedestal  = ZdcCalibPar->getPedestal();
  ZdcCalibPar<PdbPmtPeak>   *Overflow0 = ZdcCalibPar->getOverflow0();
  ZdcCalibPar<PdbPmtPeak>   *Overflow1 = ZdcCalibPar->getOverflow1();
  ZdcCalibPar<PdbPmtPeak>   *PmtGain   = ZdcCalibPar->getPmtGain();
  ZdcCalibPar<PdbPmtFitPar> *AdcGain   = ZdcCalibPar->getAdcGain();
  ZdcCalibPar<PdbPmtFitPar> *TdcGain0  = ZdcCalibPar->getTdcGain0();
  ZdcCalibPar<PdbPmtFitPar> *TdcGain1  = ZdcCalibPar->getTdcGain1();
  ZdcCalibPar<PdbPmtFitPar> *Slewing0  = ZdcCalibPar->getSlewing0();
  ZdcCalibPar<PdbPmtFitPar> *Slewing1  = ZdcCalibPar->getSlewing1();

//PHTimeStamp tStart = PHTimeStamp(2000,6,15,0,0,0);
//Pedestal->store(tStart, "pedestal");
//Overflow0->store(tStart,"overflow0");
//Overflow1->store(tStart,"overflow1");
//PmtGain->store(tStart,  "pmtgain");
//AdcGain->store(tStart,  "adc");
//TdcGain0->store(tStart, "tdc0");
//TdcGain1->store(tStart, "tdc1");
//Slewing0->store(tStart, "slewpar0");
//Slewing1->store(tStart, "slewpar1");
}
