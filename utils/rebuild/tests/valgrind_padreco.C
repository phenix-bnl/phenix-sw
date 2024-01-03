void valgrind_padreco(const int nevents=100,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libpad.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_padreco");

  Fun4AllServer* se = Fun4AllServer::instance(); 

  SubsysReco *pad = new PadReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_padreco");
  
  se->registerSubsystem(pad);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_padreco.root");
  se->registerOutputManager(out);
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(input);
  se->registerInputManager(in);
  se->run(nevents);
  se->End();

  cout << "Successfully Completed Analysis." << endl;
  delete se;
  gSystem->Exit(0);
}

