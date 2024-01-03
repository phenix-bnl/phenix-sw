
void valgrind_tofreco(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_tofreco");

  Fun4AllServer* se = Fun4AllServer::instance(); 
  SubsysReco *tof = new TofReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_tofreco");

  se->registerSubsystem(tof);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_tofreco.root");
  se->registerOutputManager(out);
  pfileopen(input);
  pidentify(0);
  prun(nevents);
  se->End();
}
