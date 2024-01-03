void valgrind_trigger(const int nevts = 1000, const char *prdffile = "/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000117455-0000.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");

  Fun4AllServer *se = Fun4AllServer::instance();

  recoConsts *rc = recoConsts::instance();

  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();

  se->registerSubsystem(head);
  se->registerSubsystem(trig);

  pfileopen(prdffile);

  prun(nevts);
  se->End();

}














