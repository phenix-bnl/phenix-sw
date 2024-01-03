void valgrind_header(const int nevents=1000, const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  Fun4AllServer *se = Fun4AllServer::instance();
  SubsysReco *head = new HeadReco();

  se->registerSubsystem(head);

  pfileopen(input);
  prun(nevents);
  se->End();
}

