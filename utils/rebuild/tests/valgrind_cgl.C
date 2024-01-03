void valgrind_cgl(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");
  cout << "Creating softlink for fieldIntegral.dat" << endl;
  gSystem->Exec("ln -s /afs/rhic.bnl.gov/phenix/software/calibration/run4/fieldIntegral.dat .");
  gSystem->Exec("mkdir ./valgrind_cgl");
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
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_cgl");


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
  se->registerSubsystem(dmp);

  Fun4AllDstOutputManager *allout  = new Fun4AllDstOutputManager("ALLOUT","valgrind_cgl.root");
  se->registerOutputManager(allout);
  
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(input);
  se->registerInputManager(in);
  se->run(nevents);

  //gBenchmark->Show("preco");
  se->End();
}

