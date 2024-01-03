void valgrind_ring(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_ring");

  cout << "Creating softlink for crk_cabling.txt" << endl;
  gSystem->Exec("ln -s /afs/rhic/phenix/software/calibration/run4/crk_cabling.txt .");
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
  SubsysReco *dch = new DchReco();
  SubsysReco *emc = new EmcReco3();
  SubsysReco *tof = new TofReco();
  SubsysReco *cgl = new CglReco();
  SubsysReco *crk = new CrkReco();
  SubsysReco *ring = new RingReco("RING");
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_ring");

  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(t0);

  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(dch);
  se->registerSubsystem(emc);
  se->registerSubsystem(tof);
  se->registerSubsystem(cgl);
  se->registerSubsystem(crk);
  se->registerSubsystem(ring);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_ring.root");
  se->registerOutputManager(out);

  pfileopen(input);
  //gBenchmark->Start("preco");
  prun(nevents);
  //gBenchmark->Show("preco");
  se->End();
//   cout << "Removing softlink for crk_cabling.txt" << endl;
//   gSystem->Exec("rm -f crk_cabling.txt");
//   cout << "Removing softlink for fieldIntegral.dat" << endl;
//   gSystem->Exec("rm -f fieldIntegral.dat");

}

