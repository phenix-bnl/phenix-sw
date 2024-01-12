//#include <libgen.h>

void PyquenJetEvents(
  const int nevents = 1000, 
  const char *outputname = "phpyquen.root"
  )
{
  gSystem->Load("libfun4all.so");	// framework + reco modules
  gSystem->Load("libPHPythia.so");
  gSystem->Load("libPHPyquen.so");
  gSystem->Load("libsimreco.so");	// framework + reco modules

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",0);

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  SubsysReco *sync = new SyncSimreco();
  se->registerSubsystem(sync);

  PHPyquen *phpyquen = new PHPyquen();
  
  // Set your own seed, otherwise, seeds from /dev/random
  //phpythia->SetSeed(1999);			
  
  se->registerSubsystem(phpyquen);

  //** A dummy (null) input is needed for the Fun4All framework
  Fun4AllDummyInputManager *in1 = new Fun4AllDummyInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  // DST output manager
  TString OUTPUT = outputname;
  Fun4AllDstOutputManager *dst_output_mgr  = new Fun4AllDstOutputManager("PHPYTHIA",OUTPUT.Data());
  dst_output_mgr->AddNode("Sync");
  dst_output_mgr->AddNode("PHPythiaHeader");
  dst_output_mgr->AddNode("PHPythia");
  se->registerOutputManager(dst_output_mgr);

  // run over all events
  se->run(nevents);  
  se->End();
}

