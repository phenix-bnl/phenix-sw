void run_pp(char *prdffile = "/common/buffer1/monitortest/keep/EVENTDATAxxx_P01-0000040463-0000.PRDFFZ")
{
  gSystem->Load("libfun4all.so");
  gROOT->ProcessLine(".L iomanager.C");
  Fun4AllServer *se = Fun4AllServer::instance();
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("PPFLAG", 1);
  rc->set_IntFlag("BFIELDFLAG", 1);
  SubsysReco *head = new HeadReco();
  SubsysReco *trig = new TrigReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  SubsysReco *ntc = new NtcReco();
  SubsysReco *tzr = new TzrReco();
  SubsysReco *t0 = new T0Reco();
  SubsysReco *pad = new PadReco();
  SubsysReco *vtx = new VtxReco("VTX1");
  SubsysReco *tec = new TecReco();
  SubsysReco *tof = new TofReco();
  SubsysReco *dch = new DchReco();
  SubsysReco *crk = new CrkReco();
  SubsysReco *emc = new EmcReco();
  SubsysReco *ert = new ErtReco();
  SubsysReco *cgl = new CglReco();
  SubsysReco *ring = new RingReco("RING");
  SubsysReco *kal = new KalFitReco();

  SubsysReco *mui = new MuiReco();
  SubsysReco *mutoo = new MutooReco();
  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(ntc);
  se->registerSubsystem(tzr);
  se->registerSubsystem(t0);
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
  se->Verbosity(0);
  iomanagerPP();
  pfileopen(prdffile);
  //gBenchmark->Start("preco");
  prun(100);
  //gBenchmark->Show("preco");
  se->EndRun(1);
}

