void valgrind_aero(const int nevents=100,
		   const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");
  cout << "Creating softlink for fieldIntegral.dat" << endl;
  gSystem->Exec("ln -s /afs/rhic/phenix/software/calibration/run4/fieldIntegral.dat .");
  gSystem->Exec("mkdir ./valgrind_aero");
  
  Fun4AllServer* se = Fun4AllServer::instance();
  se->Verbosity(1);
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig = new TrigReco();

  SubsysReco *bbc     = new BbcReco();

  SubsysReco *pad     = new PadReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *t0     = new T0Reco();
  SubsysReco *aero    = new AccReco();

  SubsysReco *dch     = new DchReco();
  SubsysReco *tof     = new TofReco();
  SubsysReco *cgl     = new CglReco();
  SubsysReco *aerocl  = new AccclusterReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_aero");


  se->registerSubsystem(head);
  se->registerSubsystem(trig);

  se->registerSubsystem(bbc);

  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(t0);
  se->registerSubsystem(aero);

  se->registerSubsystem(dch);
  se->registerSubsystem(tof);
  se->registerSubsystem(cgl);
  se->registerSubsystem(aerocl);
  se->registerSubsystem(dmp);
  
  Fun4AllDstOutputManager *allout  = new Fun4AllDstOutputManager("ALLOUT","acc.root");
  se->registerOutputManager(allout);

  pfileopen(input);
  prun(nevents);
  se->End();

}



