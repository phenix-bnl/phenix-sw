#include <string>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;

void run_clusterstability( 
     int nEvents = 100,
     const char *inputfile="EVENTDATA_P00-0000377171-0000.PRDFF",
     const char *output  ="out.root"
    )
{
  //Tell root to really crash when something goes wrong not start it's
  //signal handling.
  for (int i = 0; i < kMAXSIGNALS; i++) 
    {
      gSystem->IgnoreSignal((ESignals)i);
    }
 
//  char ifile[5000]; // + 1 for the \0 which marks the end of string
//  strcpy(ifile, inputfile);
//  strtok(ifile, "-");
//  int runnumber = atoi(strtok(0, "-"));
//  int segnumber = atoi(strtok(strtok(0, "-"), "."));


//  char output[200];
//  sprintf(output,"outfile_%010d-%04d.root",runnumber,segnumber);
//  cout<<"RUUNNIINNGG!!"<<endl;
//  cout<<output<<endl;
   
  ///////////////////////////////////////////
  // Load Libraries
  //////////////////////////////////////////

  gSystem->Load("libSvxDstQA.so");
 
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);
 
  ///////////////////////////////////////////
  // Make and register the Raw Data Checker
  //////////////////////////////////////////
  //-RawDataCheck *raw = rawdatacheck();

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

  cout<<"registering SvxClusterStabilityQA"<<endl;
  //SvxStabilityQA *svxqa = new SvxStabilityQA();
  SvxClusterStabilityQA *svxqa = new SvxClusterStabilityQA();
  svxqa->Verbosity(1);
  svxqa->Set_outname(output);
  se->registerSubsystem(svxqa);
  cout<<"finished SvxClusterStabilityQA"<<endl;

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  se->registerInputManager(in);
  se->fileopen(in->Name(), inputfile);
  cout<<"running"<<endl;
  se->run(nEvents);

  se->End();

  //SVXQA_IOManager(runnumber,segnumber); 
  
  //Fun4AllHistoManager *hm = se->getHistoManager("SVXQA");
  //if(hm) hm->dumpHistos(output);

  int evts = se->PrdfEvents();

  std::cout << "Total Events:  " << evts << std::endl;

  cout << "Successfully Completed Analysis." << endl;

  delete se;

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout<<"Fun4All successfully completed "<<endl;
  gSystem->Exit(0);

}
