void valgrind_zdcreco(const int nevents=1000,const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");
  gSystem->Exec("mkdir ./valgrind_zdcreco");

  Fun4AllServer *se = Fun4AllServer::instance();

  SubsysReco *zdc = new ZdcReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_zdcreco");
  se->registerSubsystem(zdc);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_zdcreco.root");
  se->registerOutputManager(out);

  pfileopen(input);
  prun(nevents);
  se->End();
  delete se;
}

