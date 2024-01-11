int run4ppBulkCommit(int run = -1, 
                      int by = 0, int bl = 0, int bd = 0, int bh = 0, int bm = 0, int bs = 0,
                      int ey = 0, int el = 0, int ed = 0, int eh = 0, int em = 0, int es = 0,
                      int commit = 0) {

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

  TString filename = "run4lists/run4pp/run";
  filename += run;
  filename += "_newBadROCs.txt";

  printf("Opening Bad ROC file %s...\n", (char *)filename.Data());
  PCO->FetchBadROCFromFile((char *)filename.Data());
  PHTimeStamp TSstart(by,bl,bd,bh,bm,bs);
  PHTimeStamp TSstop(ey,el,ed,eh,em,es);

  printf("\n TSstart ");
  TSstart.print();
  cout << endl;
  printf("\n TSstop  ");
  TSstop.print();
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

