void valgrind_global(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");
  gSystem->Exec("mkdir ./valgrind_global");
  
  cout << "Creating softlink for fieldIntegral.dat" << endl;
  gSystem->Exec("ln -s /afs/rhic/phenix/software/calibration/run4/fieldIntegral.dat .");
  Fun4AllServer *se = Fun4AllServer::instance();
  //se->Verbosity(1);
  recoConsts* rc= recoConsts::instance();

  SubsysReco *head = new HeadReco();
  SubsysReco *trig = new TrigReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *t0 = new T0Reco();
  SubsysReco *pad = new PadReco("PAD");
  SubsysReco *vtx = new VtxReco("VTX");
  SubsysReco *tec = new TecReco();
  SubsysReco *tof = new TofReco();
  SubsysReco *dch = new DchReco();
  SubsysReco *emc = new EmcReco3();
  SubsysReco *cgl = new CglReco();
  SubsysReco *central = new CentraltrackReco(19);
  SubsysReco *match   = new MatchrecalReco();
  SubsysReco *veto    = new ChargedvetoReco();
  SubsysReco *global  = new GlobalReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_global");
  
  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(t0);

  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(tec);
  se->registerSubsystem(tof);
  se->registerSubsystem(dch);
  se->registerSubsystem(emc);
  se->registerSubsystem(cgl);
  se->registerSubsystem(central);
  se->registerSubsystem(match);
  se->registerSubsystem(veto);
  se->registerSubsystem(global);
  se->registerSubsystem(dmp);
  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_global.root");
  se->registerOutputManager(out);

  pfileopen(input);
  //gBenchmark->Start("preco");
  prun(nevents);
  //gBenchmark->Show("preco");
  se->End();
}

