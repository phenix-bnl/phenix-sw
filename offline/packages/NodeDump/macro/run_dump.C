void run_dump(const char *infile, const int evts=100)
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libMWG_interface.so");
  gSystem->Load("libmrpc.so");
  gSystem->Load("libSvxDstQA.so");
  gSystem->Load("librecal.so");
  gSystem->Load("libphnodedump.so");

  Fun4AllServer* se = Fun4AllServer::instance();

  Dumper *dmp = new Dumper();
  gSystem->Exec("mkdir dump");
  dmp->SetOutDir("./dump");

  se->registerSubsystem(dmp);

  Fun4AllInputManager *in = new Fun4AllDstInputManager("DSTin");
  se->registerInputManager(in);
  se->fileopen("DSTin",infile);
  se->run(evts);
  se->End();
  delete se;
}
