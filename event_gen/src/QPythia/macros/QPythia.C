void QPythia(int nevents=100,
	     const std::string& configFile="phqpythia.cfg", 
	     const std::string& outfile="qpythia.root"){

  gSystem->Load("libfun4all.so");	// framework + reco modules
  gSystem->Load("libsimreco.so");	// framework + reco modules
  gSystem->Load("libPHQPythia.so");	// framework + reco modules

//   getchar();

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",246444);

  ////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

 
  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  SubsysReco *sync = new SyncSimreco();
  se->registerSubsystem(sync);

  PHQPythia *qpythia = new PHQPythia(123456789,configFile);
  se->registerSubsystem(qpythia);

  //** A dummy (null) input is needed for the Fun4All framework
  Fun4AllDummyInputManager *in1 = new Fun4AllDummyInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  TString OUTPUT = outfile;
  Fun4AllDstOutputManager *dst_output_mgr  = new Fun4AllDstOutputManager("PHPYTHIA",OUTPUT.Data());
  dst_output_mgr->AddNode("Sync");
  dst_output_mgr->AddNode("PHPythiaHeader");
  dst_output_mgr->AddNode("PHPythia");
  se->registerOutputManager(dst_output_mgr);

  // run over all events
  se->run(nevents);  
  se->End();
}
