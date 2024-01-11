void run(char *prdffile = "/phenix/data44/phnxreco/run3/ERT_electron/finished_033/va033_EVENTDATAxxx_P01-0000078838-0000-001.PRDFF", int pp = 0)
{
  gSystem->Load("libonlcalserver_funcs.so");
  gSystem->Load("libonlreco.so");
  gSystem->Load("libndst.so");
  gSystem->Load("libpreco.so");

  Reco *r = Reco::instance();      // create subsystem Calibrator object
  OnlCalServer *se = OnlCalServer::instance(); // get pointer to Server Framework
  se->setEndOfValidityRange(0);
  se->registerCalibrator(r);       // register subsystem Calibrator with Framework
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BBCCALIBVERSION", 1004);
  rc->set_IntFlag("SIMULATIONFLAG", 0);
  rc->set_IntFlag("EMBEDFLAG", 0);
  rc->set_IntFlag("PPFLAG", pp);
  rc->set_IntFlag("BFIELDFLAG", 2);
  rc->set_IntFlag("GEOMFLAG", 1);
  rc->set_IntFlag("YEAR1FLAG", 0);
  rc->set_IntFlag("PASS_MVD", 2);
  rc->set_IntFlag("BBCCALIBVERSION", 1004);
  rc->set_IntFlag("TOFTRIGGERSELECT", 0);
  rc->set_IntFlag("EVALUATIONFLAG", 0);
  SubsysReco *trig = new TrigReco();
  SubsysReco *ert = new ErtReco();
  SubsysReco *lvl2 = new Lvl2Reco();
  SubsysReco *lvl2Prim = new lvl2PrimLowOcupyReco();
  SubsysReco *head = new HeadReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  //SubsysReco *mvd = new MvdReco();
  SubsysReco *t0 = new T0Reco();
  SubsysReco *pad = new PadReco();
  SubsysReco *vtx = new VtxReco();
  SubsysReco *tec = new TecReco();
  SubsysReco *tecpid = new TecPidReco();
  SubsysReco *tof = new TofReco();
  SubsysReco *dch = new DchReco();
  SubsysReco *crk = new CrkReco();
  SubsysReco *emc = new EmcReco();
  SubsysReco *cgl = new CglReco();
  //SubsysReco *mui = new MuiReco();
  //SubsysReco *mutoo = new MutooReco();
  //SubsysReco *spin = new SpinReco();
  SubsysReco *ring = new RingReco();
  SubsysReco *central = new Centraltrackv9Reco();

  //r->registerSubsystem(trig);
  //r->registerSubsystem(ert);
  //r->registerSubsystem(head);
  //r->registerSubsystem(bbc);
  //r->registerSubsystem(zdc);
  ////r->registerSubsystem(mvd);
  //r->registerSubsystem(t0);
  //r->registerSubsystem(pad);
  //r->registerSubsystem(vtx);
  r->registerSubsystem(tec);
  //r->registerSubsystem(tof);
  //r->registerSubsystem(dch);
  //r->registerSubsystem(crk);
  //r->registerSubsystem(emc);
  //r->registerSubsystem(cgl);
  //r->registerSubsystem(tecpid);
  //// spin is just bad code and muons have to be separated from pisa
  ////   r->registerSubsystem(mui);
  ////   r->registerSubsystem(mutoo);
  ////   r->registerSubsystem(spin);
  //r->registerSubsystem(ring);
  //r->registerSubsystem(central);

  r->Verbosity(1);
  se->outfileopen("dstout.root");
  pfileopen(prdffile);

  //gBenchmark->Start("preco");
  prun(50);
  //gBenchmark->Show("preco");
  se->outfileclose();
}




