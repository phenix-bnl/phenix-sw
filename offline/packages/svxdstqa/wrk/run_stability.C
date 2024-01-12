#include <string>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;

void run_stability( 
		   const char *inputfile="/direct/phenix+prod03/phnxreco/PRDFs/run12/200CuAu/377171/EVENTDATA_P00-0000377171-0203.PRDFF",
		   const char *output="/direct/phenix+hhj/dcm07e/vtx/unstable_test/newunstabletest_wadc_377171-0203.root")
{


  int nEvents = 0;
  //Tell root to really crash when something goes wrong not start it's
  //signal handling.
  for (int i = 0; i < kMAXSIGNALS; i++) 
    {
      gSystem->IgnoreSignal((ESignals)i);
    }
 
  //  gSystem->Exec("/bin/env");
  //char ifile[5000];
  //strcpy(ifile, inputfile);
  //strtok(ifile, "-");
  int runnumber = 377171;

  cout<<"RUUNNIINNGG!!"<<endl;

   
  ///////////////////////////////////////////
  // Load Libraries
  //////////////////////////////////////////

  gSystem->Load("libSvxDstQA.so");
 
  //gROOT->ProcessLine(".L OutputManager.C");
  //gROOT->ProcessLine(".L rawdatacheck.C");

  //SetCvsTag();

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);
 
  ///////////////////////////////////////////
  // Make and register the Raw Data Checker
  //////////////////////////////////////////
  //RawDataCheck *raw = rawdatacheck();

  ///////////////////////////////////////////
  // Make the Synchronization Object
  ///////////////////////////////////////////
  SubsysReco *sync = new SyncReco();

  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *peve    = new PreviousEventReco();

  BbcReco *bbc     = new BbcReco();
  bbc->setBbcVtxError( 0.5 );
  
  SubsysReco *zdc     = new ZdcReco();
  SubsysReco *t0      = new T0Reco();
  SubsysReco *vtx     = new VtxReco();


  ////////////////////////////////// 
  // Register SubSystems 
  ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);


  se->registerSubsystem(peve);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(t0);
  se->registerSubsystem(vtx);

  SvxParManager *svxpar = new SvxParManager();
  se->registerSubsystem(svxpar);

  SvxDecode *svxdecode = new SvxDecode();
  svxdecode->includePixel(true);
  svxdecode->includeStripixel(true);
  svxdecode->setAdcOffset(24);
  svxdecode->setAdcCutoff(-24);
  se->registerSubsystem(svxdecode);

  //SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  //se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  se->registerSubsystem(svxrec);

  cout<<"registering SvxStabilityQA"<<endl;
  SvxStabilityQA *svxqa = new SvxStabilityQA();
  svxqa->Set_outname(output);
  svxqa->Verbosity(1);
  se->registerSubsystem(svxqa);
  cout<<"finished SvxStabilityQA"<<endl;

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  //in->fileopen(inputfile);
  se->registerInputManager(in);
  cout<<"registered input manager"<<endl;
  se->fileopen(in->Name(), inputfile);
  //in->AddListFile(inputfile);
  cout<<"running"<<endl;
  se->run(nEvents);

  se->End();

  int evts = se->PrdfEvents();

  std::cout << "Total Events:  " << evts << std::endl;

  cout << "Successfully Completed Analysis." << endl;

  delete se;

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout<<"Fun4All successfully completed "<<endl;
  gSystem->Exit(0);

}
