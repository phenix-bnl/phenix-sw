void readBADRocsfromDB() {

  // Executing initialization macro
  // Loading PHOOL libraries
//  gSystem->Load("libEvent.so");
//  gSystem->Load("libphool.so");
//  gSystem->Load("libWrappers.so");
//  gSystem->Load("libPhHistogramFactory.so");

//  gSystem->Load("libdcm.so");
//  gSystem->Load("libPISARoot.so");
  gSystem->Load("libphgeo.so");
//  gSystem->Load("libgea.so");
  gSystem->Load("libPgCalInstance.so");
 
  gSystem->Load("libnanoDST.so");
  gSystem->Load("libCNT.so");
  gSystem->Load("libheader.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libpad.so");

  Int_t verbose = 12;

  cout << "testing,testing.." << endl;

  PadCalibrationObject* PCO = new PadCalibrationObject();
  PCO->setDebugLevel(2);

  // for searching..
  // (year,month,day,hour,min,sec)
  PHTimeStamp TS = PHTimeStamp(2005,1,2,0,0,0);
  PCO->setTimeStamp(TS);
  PCO->FetchBadROCObjy();
  // PCO->print();
  PCO->PutBadROCToFile("test1.txt"); 

  // Take out the garbage
  delete PCO;

}
