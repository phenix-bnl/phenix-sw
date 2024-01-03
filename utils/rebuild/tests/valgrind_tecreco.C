void valgrind_tecreco(const int nevents=1000, const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000117455-0000.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_tecreco");

  Fun4AllServer* se = Fun4AllServer::instance(); 
  SubsysReco *tec = new TecReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_tecreco");

  se->registerSubsystem(tec);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_tecreco.root");
  se->registerOutputManager(out);

  pfileopen(input);
  prun(nevents);
  se->End();
}
