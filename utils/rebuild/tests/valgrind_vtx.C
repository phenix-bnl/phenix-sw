void valgrind_vtx(const int nevents=1000, const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  Fun4AllServer *se = Fun4AllServer::instance();

  SubsysReco *head = new HeadReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  SubsysReco *vtx = new VtxReco("VTX");
  se->registerSubsystem(head);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(vtx);
  pfileopen(input);
  prun(nevents);
  se->End();
}

