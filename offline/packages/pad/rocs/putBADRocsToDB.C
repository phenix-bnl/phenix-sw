void putBADRocsToDB(int commit = 0) {

  // Executing initialization macro
  gSystem->Load("libphgeo.so");
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
  PCO->FetchBadROCFromFile("run5lists/run5.start-badroc.list");
  PHTimeStamp TS1 = PHTimeStamp(2005,1,1,0,0,0);
  PHTimeStamp TS2 = PHTimeStamp();
  TS2.setToFarFuture();

  PCO->setTimeStampUpdateStart(TS1);
  PCO->setTimeStampUpdateStop(TS2);

  if(commit) {
    printf("Attempting to commit calibrations...\n");
    putStat = PCO->PutBadROCObjy();
    if (putStat) printf("\n!!PUT WORKED!!\n");
    else printf("\n!!PUT DID NOT WORK!!\n");
  } else {
    printf("Skipping calibration committing...\n");
  }

  // Take out the garbage
  delete PCO;

}
