int bulkCommitBadROCs(int run = -1, int start = 0, int stop = 0, int commit = 0) {

  gSystem->Load("libphgeo.so");
  gSystem->Load("libPgCalInstance.so");
 
  gSystem->Load("libnanoDST.so");
  gSystem->Load("libCNT.so");
  gSystem->Load("libheader.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libpad.so");

  PadCalibrationObject* PCO = new PadCalibrationObject();
  PCO->setDebugLevel(0);
  
  PHBoolean putStat;

  TString filename = "run4lists/run";
  filename += run;
  filename += "_newBadROCs.txt";

  PCO->FetchBadROCFromFile((char *)filename.Data());
  PHTimeStamp TSstart(start);
  PHTimeStamp TSstop(stop);

  printf("\n TSstart ");
  TSstart->print();
  cout << endl;
  printf("\n TSstop  ");
  TSstop->print();
  cout << endl;

  PCO->setTimeStampUpdateStart(TSstart);
  PCO->setTimeStampUpdateStop(TSstop);
  PCO->print();
  
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

