//////////////////////////////////////////
// Run15 ZDC
//////////////////////////////////////////

#include <string>
#include <sstream>
#include <iostream>
using namespace std;

// FileName: /common/c0/eventdata/EVENTDATA_P00-0000422256-0000.PRDFF
// big partition
// runnumber: 422256
// segnumber: 0
// OUTPATH: DSTOut
// FileTag: -0000422256-0000
// Big particion: /common/a0/junkdata/JUNKDATA_P00-0000419464-0000.PRDFF
// Standalone, ZDC partition: /common/standalonedata/junkdata/rc-0419848-ZDC-0.prdf
//                           /common/standalonedata/calibdata/rc-0420300-ZDC-0.prdf

void Fun4Zdc(int nEvents=0,char *inputfile="/common/c4/calibdata/CALIBDATAxxx_P01-0000147258-0000.PRDFF", string OUTPATH="DSTOut")
{
  gROOT->ProcessLine(".L MakeOutputParameters.C");  
  char outfile[255];
  int runnumber = MakeOutputParameters(inputfile, OUTPATH.c_str(), outfile );
   
  char cmd[1000];
  sprintf(cmd,"mkdir -p %s",OUTPATH.c_str());
  gSystem->Exec(cmd);

  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libnanoDST.so");
  gSystem->Load("libLPol.so");

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",runnumber);

  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  SubsysReco *head    = new HeadReco();
  // SubsysReco *sync    = new SyncReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *zdc     = new ZdcReco();
  SubsysReco *lpol    = new LPolReco(); //charge veto counters. Reinetalled for Run15 pAu - Minjung Kim

  /* Big Partition */
  se->registerSubsystem(head);
  // se->registerSubsystem(sync);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(lpol);
  

  /* ZDC standalone */
  //se->registerSubsystem(zdc);

  se->Print();

  Fun4AllDstOutputManager *dst_EVE_Manager  = new Fun4AllDstOutputManager("DST_EVE", outfile);

  // Add Nodes which you want to get
  /* Big Partition */
  dst_EVE_Manager->AddNode("RunHeader");
  dst_EVE_Manager->AddNode("EventHeader");
  // dst_EVE_Manager->AddNode("Sync");
  dst_EVE_Manager->AddNode("TrigLvl1");
  dst_EVE_Manager->AddNode("TrigRunLvl1");
  dst_EVE_Manager->AddNode("BbcOut");
  dst_EVE_Manager->AddNode("ZdcRaw"); 
  dst_EVE_Manager->AddNode("ZdcOut");  
  dst_EVE_Manager->AddNode("LPolRaw"); 

  /* ZDC standalone */
  // dst_EVE_Manager->AddNode("ZdcRaw"); 

  se->registerOutputManager(dst_EVE_Manager);

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(inputfile);
  se->registerInputManager(in);

  se->run(nEvents);
  se->End();
  cout << "Successfully Completed Analysis." << endl;

}

