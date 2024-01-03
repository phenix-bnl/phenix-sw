void valgrind_tecpid(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  cout << "Creating softlink for crk_cabling.txt" << endl;
  gSystem->Exec("ln -s /afs/rhic/phenix/software/calibration/run4/fieldIntegral.dat .");
  Fun4AllServer *se = Fun4AllServer::instance();
  //se->Verbosity(1);
  SubsysReco *head = new HeadReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  SubsysReco *t0 = new T0Reco();
  SubsysReco *pad = new PadReco("PAD");
  SubsysReco *vtx = new VtxReco("VTX");
  SubsysReco *tec = new TecReco();
  SubsysReco *tof = new TofReco();
  SubsysReco *dch = new DchReco();
  SubsysReco *cgl = new CglReco();
  SubsysReco *tecpid = new TecPidReco();
  //  SubsysReco *ring = new RingReco("RING");
  //SubsysReco *kal = new KalFitReco();

  se->registerSubsystem(head);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(t0);

  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(tec);
  se->registerSubsystem(tof);
  se->registerSubsystem(dch);
  se->registerSubsystem(cgl);
  se->registerSubsystem(tecpid);
  //se->registerSubsystem(ring);
  //  se->registerSubsystem(kal);

  pfileopen(input);
  //gBenchmark->Start("preco");
  prun(nevents);
  //gBenchmark->Show("preco");
  se->End();
}

