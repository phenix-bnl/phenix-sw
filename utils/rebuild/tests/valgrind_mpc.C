void valgrind_mpc(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_mpc");

  Fun4AllServer* se = Fun4AllServer::instance(); 

  SubsysReco *head = new HeadReco();
  SubsysReco *mpc = new MpcReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_mpc");

  se->registerSubsystem(head);
  se->registerSubsystem(mpc);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_mpc.root");
  se->registerOutputManager(out);
  pfileopen(input);
  pidentify(0);
  prun(nevents);
  se->End();
}
