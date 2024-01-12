void debugConvert(int nEvents=10, char *fileout="PHPisa.root", char *filein = "/direct/phenix+workarea/momchil/offline/framework/PisaHits.root", const Int_t debugFlag=0)
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsimreco.so");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SIMULATIONFLAG",1);
  rc->set_IntFlag("EMBEDFLAG",0);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(100);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  SubsysReco *simPad = new PadSimreco();

  se->registerSubsystem(simPad);

  ///////////////////////////////////////////
  // OutputManagers (confusingly named IOManagers, sorry for that chp)
  ///////////////////////////////////////////
  /*
  Fun4AllDstOutputManager *simManager  = new Fun4AllDstOutputManager("SIMOUT",  fileout);
  se->registerIOManager(simManager);

  simManager->AddNode("KinPisaHit");
  simManager->AddNode("PriPisaHit");
  simManager->AddNode("ZdcPisaHit");
  simManager->AddNode("AerPisaHit");
  simManager->AddNode("BbcPisaHit");
  simManager->AddNode("MvdPisaHit");
  simManager->AddNode("DchPisaHit");
  simManager->AddNode("PadPisaHit");
  simManager->AddNode("CrkPisaHit");
  simManager->AddNode("TecPisaHit");
  simManager->AddNode("TofPisaHit");
  simManager->AddNode("InrPisaHit");
  simManager->AddNode("FclPisaHit");
  simManager->AddNode("NtcPisaHit");
  simManager->AddNode("TzrPisaHit");
  simManager->AddNode("HbdPisaHit");
  simManager->AddNode("EmcPisaHit");
  simManager->AddNode("MutPisaHit");
  simManager->AddNode("MuiPisaHit");
  */

  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","DST",nEvents);
  se->registerInputManager(inMan);

  inMan->Verbosity(100);
  se->fileopen(inMan->Name(),filein);

  int iTest = 0;
  if(debugFlag) {
    //
    // Debug pause for doing attach gdb
    //
    cout << "\n debug pause " << endl;
    cin >> iTest;
    cout << "\n iTest = " << iTest << endl;
  } // used for attached debug mode

  se->run(nEvents);

  // this is only neccessary when input file is not exhausted
  se->fileclose(inMan->Name());

  se->EndRun();

  cout << "Completed reconstruction." << endl;

}
