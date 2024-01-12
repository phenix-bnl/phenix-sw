//
//  This file is a simple macro used to test the new CNT production
//  under the new framework using actively selected versions.
//
//                                        TKH 6-10-2003
//
//

void CNT_test(int nEvents=0, char *outfile="CNT.root", char *infile = "/phenix/data34/phnxreco/run2_v03_burn1/uuDST/uuDST_run2_v03_burn1-0000032043-0012.root", char* histofile= "hist.root")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libuspin.so");


  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  SubsysReco *central = new CentraltrackReco(4);
  SubsysReco *global  = new GlobalReco( GlobalReco::RUN2_AUAU_v1 );
  SubsysReco *phtrig  = new PHTrigReco();
 
  se->registerSubsystem(central);
  se->registerSubsystem(global);
  se->registerSubsystem(phtrig);

  ///////////////////////////////////////////
  // OutputManagers (confusingly named IOManagers, sorry for that chp)
  ///////////////////////////////////////////
  Fun4AllDstOutputManager *cntManager  = new Fun4AllDstOutputManager("CNTOUT",  outfile);
  se->registerIOManager(cntManager);

  cntManager->AddNode("PHGlobal");
  cntManager->AddNode("PHTrig");
  cntManager->AddNode("TrigLvl1");
  cntManager->AddNode("L2Decision");
  cntManager->AddNode("PHCentralTrack");
  cntManager->AddNode("L2ElectronCandidateMicro");
  cntManager->AddNode("L2PHIeeInvMassMicro");
  cntManager->AddNode("L2JPsieeInvMassMicro");

  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
  se->fileopen(in1->Name(),infile);
  se->run(nEvents);
  // this is only neccessary when input file is not exhausted
  se->fileclose(in1->Name());
  // this is how to open the next file
//   se->fileopen(in1->Name(),"/common/buffer1/monitortest/keep/uuDST_run2_v03_burn1-0000032043-0005.root");
//   se->run(nEvents);
//   se->fileclose(in1->Name());

  se->EndRun();
  se->dumpHistos(histofile);

  cout << "Completed reconstruction." << endl;

}
