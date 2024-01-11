void nAggro(char *inFileList="CNT_MinBias.lst", char *destFile="CNT_MinBias.root")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libonlreco.so");
  gSystem->Load("libndst.so");
  gSystem->Load("libEWG.so");
  
  gROOT->ProcessLine(".L nAggroManagers.C");
  
  //////////////////////////////////////////////////////////////////
  // Server pointer
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
  
  //////////////////////////////////////////////////////////////////
  //  INPUT MANAGER (Need one since only one input at a time...)
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
  in1->Verbosity(0);
  
  /////////////////////////////////////////////////////////////////
  //  Reco Module(s)...
  //SubsysReco *hbd = new HbdAnalyzer();
  //se->registerSubsystem(hbd);
  
  /////////////////////////////////////////////////////////////////
  //  OUTPUT MANAGER (which type depends upon first file name)...
  char dstfile[500];
  FILE *fp = fopen(inFileList,"r");
  int ncols = fscanf(fp,"%s",dstfile);
  if (ncols < 0) {
    cout << "File List was empty..." << endl;
    cout << "Terminate execution..." << endl;
    exit(1);
  }
  if     ( strncmp(dstfile,"CNT",3)==0 ) CNT_IOManager(destFile);
  elseif ( strncmp(dstfile,"HWG",3)==0 ) HWG_IOManager(destFile);
  elseif ( strncmp(dstfile,"MWG",3)==0 ) MWG_IOManager(destFile);
  elseif ( strncmp(dstfile,"PWG",3)==0 ) PWG_IOManager(destFile);
  elseif ( strncmp(dstfile,"EWG",3)==0 ) EWG_IOManager(destFile);
  else {
    cout << "Do not recognize the filetype for: " << dstfile << endl;
    cout << "Terminating..." << endl;
    exit(1);
  }
  
  ////////////////////////////////////////////////////////////////
  //  OK, now loop over all the input files...
  int i=0;
  while (1) {
    cout << "Reading file " << i++ << ": " << dstfile << " ..." << endl;

    se->fileopen("DSTin1",dstfile);    
    Fun4AllServer *se = Fun4AllServer::instance();
    se->run(0);
    
    //  Get the next file...
    int ncols = fscanf(fp,"%s",dstfile);
    if (ncols < 0) {
      break;
    }
  }
  
  se->EndRun();
  
  cout << "Successfully Completed Analysis." << endl;
  
}
