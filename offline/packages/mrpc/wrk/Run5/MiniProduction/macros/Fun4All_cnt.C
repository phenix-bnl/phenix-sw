//////////////////////////////////////////
void Fun4All_cnt(int nEvents=0)
{
  char *inputfile  = gSystem->Getenv("PRDFNAME");
  char *histofile = "hist.root"; 
  
  if (!inputfile) 
    {
      cout << "Please define input using 'setenv PRDFNAME <your_file>'" << endl;
      exit(1);
    }

  gSystem->Load("libfun4all.so");
  gROOT->ProcessLine(".L IOManager_cnt.C");
  //  gROOT->ProcessLine(".L rawdatacheck.C");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_FloatFlag("EASTMAXSAG", -0.017);
  rc->set_FloatFlag("WESTMAXSAG", -0.017);

  rc->set_IntFlag("TECACTIVE", 1); // add Tec to cgl (SL)

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Make and register the Raw Data Checker
  //////////////////////////////////////////
  //  rawdatacheckRun4();  // activates checks for all detectors

  ///////////////////////////////////////////
  // Make the Synschronization Object
  ///////////////////////////////////////////
  SubsysReco *sync = new SyncReco();

  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *zdc     = new ZdcReco();
  //  SubsysReco *fcal    = new FcalReco();
  //  SubsysReco *mvd     = new MvdReco();
  SubsysReco *t0      = new T0Reco();
  SubsysReco *pad     = new PadReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *tec     = new TecReco();
  SubsysReco *tof     = new TofReco();
  SubsysReco *aero    = new AccReco();
  SubsysReco *mrpc    = new MrpcReco();
  SubsysReco *dch     = new DchReco();
  SubsysReco *crk     = new CrkReco();
  SubsysReco *emc     = new EmcReco3();
  SubsysReco *cgl     = new CglReco();
  SubsysReco *aerocl  = new AccclusterReco();
  SubsysReco *ring    = new RingReco();
  SubsysReco *tecpid  = new TecPidReco();
  SubsysReco *central = new CentraltrackReco(17); ///need 17 for Run5 CuCu
  SubsysReco *global  = new GlobalReco(GlobalReco::RUN4_AUAU_v1);  // Run4 AuAu;

 ////////////////////////////////// 
 // Register SubSystems 
 ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  //  se->registerSubsystem(fcal);
  //  se->registerSubsystem(mvd); 
  se->registerSubsystem(t0);
  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(tec);
  se->registerSubsystem(tof);
  se->registerSubsystem(aero);
  se->registerSubsystem(mrpc);
  se->registerSubsystem(dch);
  se->registerSubsystem(crk);
  se->registerSubsystem(emc);
  se->registerSubsystem(cgl);
  se->registerSubsystem(aerocl);
  se->registerSubsystem(ring);
  se->registerSubsystem(tecpid);
  se->registerSubsystem(central);
  se->registerSubsystem(global);

  ///////////////////////////////////////////
  ///
  /// Trigger dicing
  ///////////////////////////////////////////

  TrigSelect *minBias   = new TrigSelect("MB");
  se->registerSubsystem(minBias);
  minBias->AddTrigger("MINBIAS");

  ///////////////////////////////////////////
  /// Output slicing
  ///////////////////////////////////////////
  //  Just nDST for now...
  DST_CNT_MinBias_IOManager();

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
 
  pfileopen(inputfile);
  prun(nEvents);
  se->End();
  se->dumpHistos(histofile);

  // saving job process time and output files into a ascii file
  // to save this information into DB by saskia
  //  setProcObject();

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Successfully Completed Analysis." << endl;
}
