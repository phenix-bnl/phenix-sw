void run_fun4all(char *prdffile = "data.prdf")
{
  // run EVENTDATAxxx_P01-0000027898-0000.PRDFFZ:
  //preco     : Real Time = 878.71 seconds Cpu Time = 854.23 seconds
  // run EVENTDATAxxx_P01-0000040654-0000.PRDFFZ
  // preco     : Real Time = 269.16 seconds Cpu Time = 250.41 seconds
  gSystem->Load("libfun4all.so");
  gROOT->ProcessLine(".L iomanager.C");
  //  gSystem->Load("libonlreco.so");
  Fun4AllServer *se = Fun4AllServer::instance();  
  se->Verbosity(1);
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("PPFLAG", 0);
  rc->set_IntFlag("BFIELDFLAG", 2);
  rc->set_IntFlag("BBCCALIBVERSION", 1004);
  SubsysReco *trig = new TrigReco();
//   SubsysReco *ert = new ErtReco();
//   SubsysReco *lvl2 = new Lvl2Reco();
   SubsysReco *bbc = new BbcReco();
   SubsysReco *zdc = new ZdcReco();
   SubsysReco *head = new HeadReco();
//   //SubsysReco *mvd = new MvdReco();
//   SubsysReco *ntc = new NtcReco();
    SubsysReco *t0 = new T0Reco();
    SubsysReco *pad = new PadReco();
    SubsysReco *vtx = new VtxReco("VTX1");
    SubsysReco *tec = new TecReco();
    SubsysReco *tof = new TofReco();
    SubsysReco *dch = new DchReco();
    SubsysReco *crk = new CrkReco();
    SubsysReco *emc = new EmcReco();
    SubsysReco *cgl = new CglReco();
    SubsysReco *ring = new RingReco("RING");
    //    SubsysReco *kal = new KalFitReco();

    //    SubsysReco *mui = new MuiReco();
//   SubsysReco *mutoo = new MutooReco();
//   SubsysReco *spin = new SpinReco();
  se->registerSubsystem(trig);
//   //se->registerSubsystem(lvl2);
   se->registerSubsystem(bbc);
   se->registerSubsystem(zdc);
   se->registerSubsystem(head);
//   //se->registerSubsystem(mvd);
//   se->registerSubsystem(ntc);
    se->registerSubsystem(t0);
    se->registerSubsystem(pad);
    se->registerSubsystem(vtx);
    se->registerSubsystem(tec);
    se->registerSubsystem(tof);
    se->registerSubsystem(dch);
    se->registerSubsystem(crk);
    se->registerSubsystem(emc);
    se->registerSubsystem(cgl);
    se->registerSubsystem(ring);
    //    se->registerSubsystem(kal);
  // spin is just bad code and muons have to be separated from pisa
  //  se->registerSubsystem(mui);
  //   se->registerSubsystem(mutoo);
  //   se->registerSubsystem(spin);
  se->Verbosity(1);
  iomanager1();
  iomanager2();
  pfileopen(prdffile);
  //gBenchmark->Start("preco");
  prun(100);
  //gBenchmark->Show("preco");
  se->EndRun(1);
}

