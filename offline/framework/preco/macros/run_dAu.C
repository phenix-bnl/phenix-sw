void run_dAu(char *prdffile = "/common/buffer1/monitortest/keep/EVENTDATAxxx_P01-0000076050-0000.PRDFFZ")
{
  // run EVENTDATAxxx_P01-0000027898-0000.PRDFFZ:
  //preco     : Real Time = 878.71 seconds Cpu Time = 854.23 seconds
  // run EVENTDATAxxx_P01-0000040654-0000.PRDFFZ
  // preco     : Real Time = 269.16 seconds Cpu Time = 250.41 seconds
 
  //gSystem->Load("libphnodedump.so");
  gSystem->Load("libfun4all.so");
  gROOT->ProcessLine(".L iomanager.C");
  gROOT->ProcessLine(".L rawdatacheck.C");
  gROOT->ProcessLine(".L trigselect.C");
  
  //  gSystem->Load("libonlreco.so");
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(1);
  recoConsts *rc = recoConsts::instance();
  rawdatacheckdAu();
  SubsysReco *head = new HeadReco();
  SubsysReco *trig = new TrigReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  SubsysReco *ntcp = new NtcpReco();
  SubsysReco *t0 = new T0Reco();
  SubsysReco *fcal = new FcalReco("FCAL");
  SubsysReco *pad = new PadReco("PAD");
  SubsysReco *vtx = new VtxReco("VTX1");
  SubsysReco *tec = new TecReco();
  SubsysReco *tof = new TofReco();
  SubsysReco *dch = new DchReco();
  SubsysReco *crk = new CrkReco();
  SubsysReco *emc = new EmcReco3();
  SubsysReco *ert = new ErtReco();
  SubsysReco *cgl = new CglReco();
  SubsysReco *ring = new RingReco("RING");
  SubsysReco *kal = new KalFitReco();
  SubsysReco *mui = new MuiReco();
  SubsysReco *mutoo = new MutooReco();

  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  // create and register trigger selectors
  // they need to run after the trigger reco since
  // triggerhelpoer needs the output from it
  trigselmuon();
  trigselgamma();
  trigselelectron();
  trigselminbias();
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(ntcp);
  se->registerSubsystem(t0);
  se->registerSubsystem(fcal);
  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(tec);
  se->registerSubsystem(tof);
  se->registerSubsystem(dch);
  se->registerSubsystem(crk);
  se->registerSubsystem(emc);
  se->registerSubsystem(ert);
  se->registerSubsystem(cgl);
  se->registerSubsystem(ring);
  se->registerSubsystem(kal);
  se->registerSubsystem(mui);
  se->registerSubsystem(mutoo);
  iomanagerdAu("dstdAu.proot");
  iomanagerMuon("dstdAuMuon.proot");
  iomanagerGamma("dstdAuGamma.proot");
  iomanagerElectron("dstdAuElectron.proot");
  iomanagerMinBias("dstdAuMinBias.proot");
  pfileopen(prdffile);
  //gBenchmark->Start("preco");
  prun(100);
  //gBenchmark->Show("preco");
  se->EndRun(1);
}

