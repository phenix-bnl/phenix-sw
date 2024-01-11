//////////////////////////////////////////
// Run11 ZDC
//////////////////////////////////////////

#include <string>
#include <sstream>
#include <iostream>
using namespace std;

void Fun4Zdc(int nEvents=0,char *inputfile="/common/c4/calibdata/CALIBDATAxxx_P01-0000147258-0000.PRDFF",char *outputPath="./")
{
  string FileName = inputfile;
  int runnumber;
  int segnumber;
  if(FileName.find("rc-")<10000)
    {
      string rn=FileName.substr(FileName.find("rc-")+3,7);
      runnumber=atoi(rn.c_str());
      segnumber=0;
    }
  else
    {
      int length = FileName.size();
      string FileTag = FileName.substr(length-22,16);
      istringstream RUNNUMBER(FileTag.substr(1,10));
      RUNNUMBER >> runnumber;
      istringstream SEGNUMBER(FileTag.substr(12,4));
      SEGNUMBER >> segnumber;
    }

  TString histofile = "hist"; histofile += runnumber;
  histofile += "_"; histofile += segnumber; histofile += ".root";

  // Use the Filetag to make an output path based upon 
  // Runnumber like "./run_0000154000_155000/"
  int lowrun  = (runnumber/1000)*1000;
  int highrun = lowrun + 1000;
  char dir[500];
  sprintf(dir,"run_%010d_%010d/",lowrun,highrun);
  string PATH(outputPath);
  string DIR(dir);
  string OUTPATH = PATH + DIR;


  //gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libnanoDST.so");
  //gSystem->Load("libdstqa.so");      // QA, CMV 02/22/05


  gROOT->ProcessLine(".L IOManager.C");
  //gROOT->ProcessLine(".L rawdatacheck.C");
  //gROOT->ProcessLine(".L QA.C");
  
  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  //rc->set_IntFlag("RUNNUMBER",318117);
  rc->set_IntFlag("RUNNUMBER",runnumber);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Make and register the Raw Data Checker
  //////////////////////////////////////////
  //rawdatacheck();

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
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *zdc     = new ZdcReco();
  //SubsysReco *fcal    = new FcalReco();
  //SubsysReco *t0      = new T0Reco();
  //SubsysReco *vtx     = new VtxReco();
  //SubsysReco *global  = new GlobalReco(); 

  //SubsysReco *physqa  = new PhysicsqaReco();
  //SubsysReco *spin    = new SpinReco();
  //SubsysReco *lpc     = new lpcReco();  

  // Trigger Selection

  ////////////////////////////////// 
  // Register SubSystems 
  ////////////////////////////////// 

  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);
  se->registerSubsystem(peve);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  //se->registerSubsystem(fcal);
  //se->registerSubsystem(t0);

  //se->registerSubsystem(vtx);
  
  //se->registerSubsystem(spin);
  //se->registerSubsystem(lpc);
  //se->registerSubsystem(physqa);
  
  //se->registerSubsystem(global);

  se->Print();

  //QAInit();

  ///////////////////////////////////////////
  /// Define the triggers
  ///////////////////////////////////////////

/*
  mpctrig->AddTrigger("MINBIAS");
  mpctrig->AddTrigger("BBCLL1(noVertexCut)");
  mpctrig->AddTrigger("ZDCLL1wide");
  mpctrig->AddTrigger("ZDCLL1narrow");
  mpctrig->AddTrigger("ZDCN||ZDCS");
  mpctrig->AddTrigger("BBCLL1(noVtx)&(ZDCN|ZDCS)");
  mpctrig->AddTrigger("MPC_4x4A");
  mpctrig->AddTrigger("MPC_2x2(PT)");
  mpctrig->AddTrigger("MPC_4x4C&ERTLL1_2x2");
*/

/*
  ppg->AddVetoTrigger("PPG(Laser)");
  ppg->AddVetoTrigger("PPG(Pedestal)");
  ppg->AddVetoTrigger("PPG(Test Pulse)");
  ppg->SetReturnCode("ABORT");
*/
  
  ///////////////////////////////////////////
  /// Output slicing
  ///////////////////////////////////////////

  DST_EVE_IOManager(OUTPATH.c_str(), FileTag.c_str());

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(inputfile);
  se->registerInputManager(in);
  se->run(nEvents);


  //pfileopen(inputfile);
  //prun(nEvents);

  se->End();
  //se->dumpHistos(histofile.Data());

  //QA_IOManager(OUTPATH.c_str(), FileTag.c_str());
  //RunSummary(OUTPATH.c_str(), FileTag.c_str());

  cout << "Successfully Completed Analysis." << endl;

}
