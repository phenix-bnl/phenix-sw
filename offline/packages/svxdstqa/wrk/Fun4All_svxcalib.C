#include <string>
#include <sstream>
#include <iostream>

using namespace std;

void Fun4All_svxcalib
(int nEvents=0, 
const char *inputfile="/direct/phenix+zdata03/phnxreco/VTX/prdf/eventdata/EVENTDATA_P00-0000363227-0000.PRDFF")
{
  //Tell root to really crash when something goes wrong not start it's
  //signal handling.
  for (int i = 0; i < kMAXSIGNALS; i++)
    {
      gSystem->IgnoreSignal((ESignals)i);
    }

  char ifile[strlen(inputfile)+1]; // + 1 for the \0 which marks the end of string
  strcpy(ifile, inputfile);
  strtok(ifile, "-");
  int runnumber = atoi(strtok(0, "-"));
  int segnumber = atoi(strtok(strtok(0, "-"), "."));

  ///////////////////////////////////////////
  // Load Libraries
  //////////////////////////////////////////

  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libcompactCNT.so");
  gSystem->Load("libSvxDstQA.so");
  
// can we check the returns here?
  gROOT->ProcessLine(".L OutputManager.C");
  //gROOT->ProcessLine(".L rawdatacheck.C");

 ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  /*
  rc->set_IntFlag("PADVTX_PC3_MULT_CUT",10);  // padvtx cut
  SetCvsTag();
  */
  rc->set_IntFlag("SVXACTIVE", 1);


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
  TrigSelect *ppg     = new TrigSelect("PPG");
  SubsysReco *peve    = new PreviousEventReco();
  BbcReco *bbc     = new BbcReco();
  bbc->setBbcVtxError( 0.5 );
  

  SubsysReco *vtx     = new VtxReco();
  se->registerSubsystem(vtx);
  ////////////////////////////////// 
  // Register SubSystems 
  ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);
  // Register the PPG veto trigger to get rid of those events early
  se->registerSubsystem(ppg);
  se->registerSubsystem(peve);
  se->registerSubsystem(bbc);

  SvxParManager *svxpar = new SvxParManager();
  // svxpar->Verbosity(1);
  svxpar->set_ReadGeoParFromFile(0);
  svxpar->set_OffsetVtxToCnt (0.0, 0.0, 0.0);
  svxpar->set_OffsetEastToWest(0.0229, 0.0129, 0.0);
  svxpar->set_BeamCenter(0.2723, 0.1362);
  se->registerSubsystem(svxpar);
  
  SvxDecode *svxdecode = new SvxDecode();
  svxdecode->includePixel(true);
  svxdecode->includeStripixel(true);
  svxdecode->setAdcOffset(24);
  svxdecode->setAdcCutoff(0);
  se->registerSubsystem(svxdecode);

  SvxStripFindHotDead *sana = new SvxStripFindHotDead();
  sana->setRunFlag(SvxStripFindHotDead::Run12);
  sana->Load_ThresholdFile("threshold.h");
  sana->setOutputName(MakeOutput(runnumber,segnumber,"VtxStripDeadChannels"));
  sana->set_MarkType(1);
  sana->set_ThresholdShift(24);
  se->registerSubsystem(sana);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  //svxrec->Load_ThresholdFile("/common/s2/Subsystems/hachiya/source/svx/wrk/threshold.h");
  svxrec->set_UseStripThresholdDatbase(true);
  //svxrec->set_StripixelAdcThreshold(0);
  svxrec->set_StripixelAdcSumThreshold(0);
  se->registerSubsystem(svxrec);

  SubsysReco *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  se->registerSubsystem(svxvtxseedfinder);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
//  svxstandalone->setWindowScale(3);
//  svxstandalone->setPPFlag(true);
  se->registerSubsystem(svxstandalone);

  SubsysReco *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

  // QA
  SvxDstQA *ana = new SvxDstQA(MakeOutput(runnumber,segnumber,"SvxDstQA"));
  SvxFillHisto *pxlhisto = new SvxFillHistoPixel();
  ana->registerFillHisto(pxlhisto);
  SvxFillHisto *stphisto = new SvxFillHistoStrip();
  ana->registerFillHisto(stphisto);
  SvxFillHisto *beamhisto = new SvxFillHistoBeampro();
  ana->registerFillHisto(beamhisto);
  se->registerSubsystem(ana);
 


  //////////////////////////////////
  // Define the triggers
  //////////////////////////////////
  ppg->AddVetoTrigger("PPG(Laser)");
  ppg->AddVetoTrigger("PPG(Pedestal)");
  ppg->AddVetoTrigger("PPG(Test Pulse)");
  ppg->SetReturnCode("ABORT");

  //////////////////////////////////
  // SvxQA Histogram
  //////////////////////////////////

  DST_SVX(runnumber,segnumber,"DST_SVX");

  string histofile = MakeOutput(runnumber,segnumber,"Histos");
  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
 

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(inputfile);
  se->registerInputManager(in);
  se->run(nEvents);


  se->End();

  se->dumpHistos(histofile);

  int evts = se->PrdfEvents();
  std::cout << "Total Events:  " << evts << std::endl;
 
  char sql[1000];
  sprintf(sql,"echo \"update %s set nevents=%d,time='now' where runnumber=%d and sequence=%d and prodtag = \'%s\';\" | isql phnxreco_odbc",gSystem->Getenv("PRODDBTABLE"),evts,runnumber,segnumber,gSystem->Getenv("PRODTAG"));
  std::cout << "Update DB statement: " << sql << std::endl;
  gSystem->Exec(sql);
 
  FileSummary();


  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Successfully Completed Analysis." << endl;

  delete se;

  PHTimeServer::get()->print_stat();

  cout<<"Fun4All successfully completed "<<endl;
  gSystem->Exit(0);
}
