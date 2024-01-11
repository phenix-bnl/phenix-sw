void mpchilo(unsigned int nevents = 0, char *inFile = "dst.list")
{

  //gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  //gSystem->Load("libfun4all.so");	// framework only
  gSystem->Load("libmpc.so");
  gSystem->Load("libmpccalib.so");

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  SubsysReco *mpchilo = new MpcHiLo();
  se->registerSubsystem(mpchilo);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  /////////////////////////////////////////////////////////////////
  // Now add your file lists
  TString fname = inFile;
  if ( fname.EndsWith(".root") ) in1->AddFile( fname.Data() );
  else                           in1->AddListFile( fname.Data() );

  se->run(nevents);  // run over all events
  se->End();

}

