
#include <string>
#include <sstream>

//////////////////////////////////////////
void valgrind_Fun4Everyone(int nEvents=100, 
	     char *inputfile="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000161598-0000.PRDFF",
	     char* outputPath="./")
{
  cout << "The input File Name is: " << inputfile << endl;

  // Parse the end characters of the filename to get FileTag, Runnumber and Segment Number
  // Example:
  //   --FileTag looks like -0000154354-0251
  //     From RunNumber 154354
  //          SegNumber 251
  string FileName = inputfile;
  int length = FileName.size();
  string FileTag = FileName.substr(length-22,16);
  stringstream RUNNUMBER(FileTag.substr(1,10));
  int runnumber;
  RUNNUMBER >> runnumber;
  stringstream SEGNUMBER(FileTag.substr(12,4));
  int segnumber;
  SEGNUMBER >> segnumber;

  cout << runnumber << "," << segnumber << endl;

  // Use the Filetag to make an output path based upon 
  // Runnumber like "./run_0000154000_155000/"
  int lowrun  = (runnumber/1000)*1000;
  int highrun = lowrun + 1000;
  char dir[500];
  sprintf(dir,"run_%010d_%010d/",lowrun,highrun);
  string PATH(outputPath);
  string DIR(dir);
  string OUTPATH = PATH + DIR;

  //  Back to your normally scheduled program
  char *histofile = "hist.root"; 
  char *cvsenvtag = "run05_production_B_04";

  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libmutoo_subsysreco.so");
  gSystem->Load("libEWG.so");  //  Load
  gSystem->Load("libHWG.so");  //  the
  gSystem->Load("libMWGOO.so");  //  the
  gSystem->Load("libdstqa.so"); // QA, CMV 02/22/05
  gSystem->Load("liblvl2.so"); // lvl2, CMV 02/24/05
  gSystem->Load("libFilterReco.so"); // Hard
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_Fun4Everyone");

  gROOT->ProcessLine(".L IOManager.C");
  gROOT->ProcessLine(".L rawdatacheck.C");
  gROOT->ProcessLine(".L QA.C");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_CharFlag("CVSTAG",cvsenvtag);

  rc->set_FloatFlag("EASTMAXSAG", -0.017);
  rc->set_FloatFlag("WESTMAXSAG", -0.017);

  rc->set_IntFlag("TECACTIVE", 1); // add Tec to cgl (SL)
  
  // require muon parameters printout
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);

  //set Lvl2 flags (CMV 02/24/05, from Tony F.)

  rc->set_IntFlag("LVL2_REAL_DATA",1); // not a pisa file 
  rc->set_IntFlag("LVL2_YEAR",5);   //.. run 4 database files and triggers
  rc->set_CharFlag("Run2Lvl2AlgoName", "");   // means take ALL data events
  rc->set_IntFlag("LVL2_USE_ASCII_DB",1);  // Read from directory if 1, Postgres if 0
  // For lvl1/lvl,2 trigger setup
  rc->set_CharFlag("TRIGSETUPFILE","Run5CuCu_62gev_TriggerSetupFile"); 
  rc->set_CharFlag("LVL2_DB_DIR","/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN5_REAL"); 
  rc->set_IntFlag("FORCE_LVL2",1); // force Lvl2 to run

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Make and register the Raw Data Checker
  //////////////////////////////////////////
  rawdatacheckRun4();  // activates checks for all detectors

  ///////////////////////////////////////////
  // Make the Synschronization Object
  ///////////////////////////////////////////
  SubsysReco *sync = new SyncReco();


  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();

  
  PadRemoveReset *padrem1 = new PadRemoveReset("PADREMOVE");
  padrem1->SetReturnCode("DISCARD");
  PadRemoveReset *padrem2 = new PadRemoveReset("PADREMOVESEL");
  padrem2->SetReturnCode("DISCARD");
  padrem2->SelectBad();
  

  SubsysReco *lvl2reco = new Lvl2Reco(); // Run lvl2 triggers, or get lvl2 packets from PRDF
  SubsysReco *lvl2statseval = new Lvl2StatsEval();  // Make a summary table of lvl2 decision statistics
  SubsysReco *peve    = new PreviousEventReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *zdc     = new ZdcReco();
  SubsysReco *ert     = new ErtReco();
  SubsysReco *fcal    = new FcalReco();
  SubsysReco *t0      = new T0Reco();
  SubsysReco *pad     = new PadReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *tec     = new TecReco();
  SubsysReco *tof     = new TofReco();
  SubsysReco *aero    = new AccReco();
  SubsysReco *dch     = new DchReco();
  SubsysReco *crk     = new CrkReco();
  SubsysReco *emc     = new EmcReco3();
  SubsysReco *cgl     = new CglReco();
  SubsysReco *aerocl  = new AccclusterReco();
  SubsysReco *mrpc    = new MrpcReco();
  SubsysReco *ring    = new RingReco();
  SubsysReco *tecpid  = new TecPidReco();
  SubsysReco *central = new CentraltrackReco(19); ///need 16 for Run5; 19 for pro.69
  SubsysReco *veto    = new ChargedvetoReco();
  SubsysReco *hpdst   = new FilterReco("EVENTandTRACK"); // event and track filter
 
  PHInclusiveNanoCuts *EWGcuts = new EWGInclusiveNanoCutsv2();
  (dynamic_cast<EWGInclusiveNanoCutsv2*>(EWGcuts))->set_vertexcut(50.);
  (dynamic_cast<EWGInclusiveNanoCutsv2*>(EWGcuts))->set_eoverpmin(-9999.);
  (dynamic_cast<EWGInclusiveNanoCutsv2*>(EWGcuts))->set_eoverpmax(9999.);
  (dynamic_cast<EWGInclusiveNanoCutsv2*>(EWGcuts))->set_emcPhicut(9999.);
  (dynamic_cast<EWGInclusiveNanoCutsv2*>(EWGcuts))->set_emcZcut(9999.);
  SubsysReco *EWG     = new WGReco(EWGcuts,"EWGCentralTrack");

  PHInclusiveNanoCuts *HWGcuts = new HWGInclusiveNanoCutsv2();
  SubsysReco *HWG     = new WGReco(HWGcuts,"HWGCentralTrack");

  // muon subsystems
  SubsysReco *unpack = new MuonUnpackPRDF();  // prdf muon unpacker
  unpack->Verbosity(0);
  SubsysReco *muioo = new MuiooReco();        // muid reconstruction
  SubsysReco *mutoo = new MuonDev();          // mutr reconstruction
  SubsysReco *mwg = new MWGOOReco( new MWGInclusiveNanoCutsv2() );  // muon nanoDST

  MuidEffic *muid_effic = new MuidEffic();
  muid_effic->set_excluded_gap(4);

  MuonReco *muon = new MuonReco();
  muon->set_n_LL1_hit_required(4);	 
  muon->SetDestinationFile("Mu_DST.root");
  

  SubsysReco *global  = new GlobalReco();

  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_Fun4Everyone");

 ////////////////////////////////// 
 // Register SubSystems 
 ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(sync);

  se->registerSubsystem(padrem1);
  se->registerSubsystem(padrem2);

  se->registerSubsystem(trig);
  se->registerSubsystem(lvl2reco); // lvl2 changes
  se->registerSubsystem(lvl2statseval); // lvl2 changes

  se->registerSubsystem(peve);

  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(ert);
  se->registerSubsystem(fcal);
  se->registerSubsystem(t0);
  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(tec);
  se->registerSubsystem(tof);
  se->registerSubsystem(aero);
  se->registerSubsystem(dch);
  se->registerSubsystem(crk);
  se->registerSubsystem(emc);
  se->registerSubsystem(mrpc);
  se->registerSubsystem(cgl);
  se->registerSubsystem(aerocl);
  se->registerSubsystem(ring);
  se->registerSubsystem(tecpid);
  se->registerSubsystem(central);
  se->registerSubsystem(veto);
  
  // muon subsystems
  se->registerSubsystem( unpack );
  se->registerSubsystem( muioo );
  se->registerSubsystem( mutoo );
  se->registerSubsystem( mwg );
//  se->registerSubsystem( muid_effic );
  se->registerSubsystem( muon );

  se->registerSubsystem(global);

  se->registerSubsystem(HWG);
  se->registerSubsystem(EWG);

  se->registerSubsystem(hpdst);
  se->registerSubsystem(dmp);

  se->Print();

  QAInit();

  //////////////////////////////////////////
  // Trigger Selection (MINBIAS ONLY)
  //////////////////////////////////////////

  TrigSelect *minBias = new TrigSelect("MB");
  TrigSelect *others = new TrigSelect("OTHERS");
  TrigSelect *ppg = new TrigSelect("PPG");

  se->registerSubsystem(minBias);
  se->registerSubsystem(others);
  se->registerSubsystem(ppg);

  minBias->AddTrigger("MINBIAS");

  others->AddVetoTrigger("MINBIAS");  
  others->AddTrigger("BBCLL1(noVertexCut)");
  others->AddTrigger("ERTLL1_2x2");
  others->AddTrigger("ERT_2x2");
  others->AddTrigger("ERTLL1_4x4b");
  others->AddTrigger("ZDCNS");
  others->AddTrigger("ZDCLL1narrow");
  others->AddTrigger("ZDCLL1wide");
  others->AddTrigger("ZDCLL1N");
  others->AddTrigger("ZDCLL1S");
  others->AddTrigger("Clock");

  ppg->AddVetoTrigger("PPG(Pedestal)");
  ppg->AddVetoTrigger("PPG(Test Pulse)");
  ppg->AddVetoTrigger("PPG(Laser)");
  ppg->SetReturnCode("ABORT");


  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
 
  pfileopen(inputfile);

  prun(nEvents);
  se->End();
  se->dumpHistos(histofile);

  QA_IOManager(OUTPATH.c_str(), FileTag.c_str());
	
  Mu_DST_IOManager(OUTPATH.c_str(), FileTag.c_str());
//  MuidEff_IOManager(OUTPATH.c_str(), FileTag.c_str());

  minBias->Print();

  cout << "#####" << endl;

  others->Print();

  cout << "#####" << endl;

  ppg->Print();

  RunSummary(OUTPATH.c_str(), FileTag.c_str());

  se->Print("OUTPUTMANAGER%EVENTSWRITTEN");


  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
  cout << "Successfully Completed Analysis." << endl;

  //Update the database with the number of procesed events...
  //int evts = se->TotalEvents();
  //char sql[500];
  //sprintf(sql,"psql mlp -c \"update  cohoproduction set minbias=%d where filename='%s';\"",evts,inputfile);
  //gSystem->Exec(sql);

}
