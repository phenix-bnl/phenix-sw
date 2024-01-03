void Readability_Test(const int nevents=10,
		      const char* input="dst.root")
{
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libphnodedump.so");

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 

  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *dump    = new Dumper();

  se->registerSubsystem(dump);
  Fun4AllInputManager *in = new Fun4AllDstInputManager("DSTIN");
  in->fileopen(input);
  se->registerInputManager(in);
  se->run(nevents);
  se->End();

  cout << "Successfully Completed Analysis." << endl;
  delete se;
  gSystem->Exit(0);
}
