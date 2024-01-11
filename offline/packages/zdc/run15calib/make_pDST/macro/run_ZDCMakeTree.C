void run_ZDCMakeTree(int runnumber=422204, int nrun=100000){
	
  char ndstdir[255]="/direct/phenix+u/workarea/minjung/offline/packages/zdc/run15calib/make_nDST/DSTOut/";

  char input[255];
  sprintf(input,"%s/DST_EVE_ZDCSMD-0000%d-0000.root",ndstdir,runnumber);
  char output[255];
  sprintf(output,"pDST%d.root",runnumber);

  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libZDCMakeTree.so");
 
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
  
  ZDCMakeTree *ana = new ZDCMakeTree();
  ana->SetOutputName(output);
  se->registerSubsystem(ana);
 
  Fun4AllDstInputManager* in = new Fun4AllDstInputManager("DSTIN1");
  se->registerInputManager(in);
  in->Verbosity(0);

  in->AddFile(input);

  se->run(nrun);
  se->End();
}
