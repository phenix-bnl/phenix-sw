void valgrind_kalman(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");
  cout << "Creating softlink for fieldIntegral.dat" << endl;
  gSystem->Exec("ln -s /afs/rhic/phenix/software/calibration/run4/fieldIntegral.dat .");
  gSystem->Exec("mkdir ./valgrind_kalman");

  Fun4AllServer *se = Fun4AllServer::instance();
  //se->Verbosity(1);
  recoConsts* rc= recoConsts::instance();
 ///////////////////////////////////////////
  // Kalman Flags
  //////////////////////////////////////////

  rc->set_FloatFlag("KALPMIN",0.400);
  rc->set_IntFlag("KALFILTERDCUV",1);
  rc->set_IntFlag("KALFIT",1);

  rc->set_IntFlag("KALUSEDCHX1X2",1);
  rc->set_IntFlag("KALUSEDCHUV",1);
  rc->set_IntFlag("KALUSEPC1",1);
  rc->set_IntFlag("KALUSEPC2",1);
  rc->set_IntFlag("KALUSEPC3",1);
  rc->set_IntFlag("KALUSETEC",1);
  rc->set_IntFlag("KALUSETOF",0);
  rc->set_IntFlag("KALUSEEMC",0);
  rc->set_IntFlag("KALUSESVX",0);
  rc->set_IntFlag("KALSVXASSOC",0);
  
  // The sign of the scale factor is set by the currents
  rc->set_FloatFlag("PHFIELDMAPSCALE",1.0);
  // Only one of these can be set
  rc->set_IntFlag("KALFORCEPROTON",0);
  rc->set_IntFlag("KALFORCEELECTRON",0);
  rc->set_IntFlag("KALFORCEKAON",0);
  

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
  SubsysReco *kal = new KalFitReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_kalman");
 

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
  se->registerSubsystem(kal);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_kalman.root");
  se->registerOutputManager(out);
  
  pfileopen(input);
  //gBenchmark->Start("preco");
  prun(nevents);
  //gBenchmark->Show("preco");
  se->End();
}

