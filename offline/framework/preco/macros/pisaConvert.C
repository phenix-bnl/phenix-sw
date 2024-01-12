void pisaConvert(int nEvents=10, char *fileout="PHPisa.root", char *filein = "/direct/phenix+workarea/momchil/offline/framework/PisaHits.root")
{
  gSystem->Load("libfun4all.so");


  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  //  recoConsts *rc = recoConsts::instance();

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(100);

//   SubsysReco *central = new CentraltrackReco(4);
//   SubsysReco *global  = new GlobalReco( GlobalReco::RUN2_AUAU_v1 );
//   SubsysReco *phtrig  = new PHTrigReco();
 
//   se->registerSubsystem(central);
//   se->registerSubsystem(global);
//   se->registerSubsystem(phtrig);

  ///////////////////////////////////////////
  // OutputManagers (confusingly named IOManagers, sorry for that chp)
  ///////////////////////////////////////////
  Fun4AllDstOutputManager *simManager  = new Fun4AllDstOutputManager("SIMOUT",  fileout);
  se->registerIOManager(simManager);

   simManager->AddNode("PISA");
   simManager->AddNode("GEA");
   simManager->AddNode("EVA");
//   simManager->AddNode("RUN");
//   simManager->AddNode("PAR");

  simManager->AddNode("fkin");
  simManager->AddNode("primary");
  simManager->AddNode("pythia");
  simManager->AddNode("verghit");
  simManager->AddNode("bbcghit");
  simManager->AddNode("dcghit");
  simManager->AddNode("pc1ghit");
  simManager->AddNode("pc2ghit");
  simManager->AddNode("pc3ghit");
  simManager->AddNode("crkghit");
  simManager->AddNode("tecghit");
  simManager->AddNode("tofghit");
  simManager->AddNode("dEmcGeaHit");
  simManager->AddNode("munhits");
  simManager->AddNode("mumhits");
  simManager->AddNode("ZdcGeaHits");
  simManager->AddNode("TzrGeaHits");
  simManager->AddNode("header");
//   simManager->AddNode("emcpar");

  simManager->AddNode("KinPisaHit");
  simManager->AddNode("PriPisaHit");
  simManager->AddNode("ZdcPisaHit");
  simManager->AddNode("AerPisaHit");
.q  simManager->AddNode("BbcPisaHit");
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

  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","DST",nEvents);
  se->registerInputManager(inMan);
  inMan->Verbosity(100);
  //  inMan->setBeginEvent(1);
  //  inMan->setVersion(1);
  se->fileopen(inMan->Name(),filein);
  se->run(nEvents);
  // this is only neccessary when input file is not exhausted
  se->fileclose(inMan->Name());
  // this is how to open the next file
//   se->fileopen(inMan->Name(),"/common/buffer1/monitortest/keep/uuDST_run2_v03_burn1-0000032043-0005.root");
//   se->run(nEvents);
//   se->fileclose(inMan->Name());

  se->EndRun();
//  se->dumpHistos(histofile);

  cout << "Completed reconstruction." << endl;

}
