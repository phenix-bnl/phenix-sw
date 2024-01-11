void bulkReadBadROCs(int run = -1, int start = 0) {

  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libgea.so");
  gSystem->Load("libPdbCal.so");
 
  gSystem->Load("libpad.so");

  PadCalibrationObject* PCO = new PadCalibrationObject();

  PHTimeStamp TS(start);
  PCO->setTimeStamp(TS);
  PCO->FetchBadROCObjy();

  TString filename = "run4lists.verify/run";
  filename += run;
  filename += "_newBadROCs.txt";

  PCO->PutBadROCToFile(filename.Data()); 

  // Take out the garbage
  delete PCO;

}
