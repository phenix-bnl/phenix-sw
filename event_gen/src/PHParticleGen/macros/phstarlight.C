//
// This macro converts a Starlight output file (which has to be called slight.out) into a
// PHPythia format file (which is fun4all compliant)
//
void phstarlight(const int nevents = 0, const char *outputname = "phstarlight.root")
{
  //gSystem->Load("libPythia6.so");
  gSystem->Load("libfun4all.so");	// framework + reco modules
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPHPythia.so");
  //gSystem->Load("libPHPyTrigger.so");	// only needed if triggering
  gSystem->Load("libPHParticleGen.so");
  //gSystem->Load("libPHParticleCustomGen.so");	// only needed for custom generator

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",0);

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...

  PHParticleGen* ss = new PHParticleGen();

  //*** Module to read in starlight file
  TStarlightGenerator *spg = new TStarlightGenerator();
  ss->SetGenerator(spg);
  spg->OpenStarlightFile("slight.out");

  //ss->SetSeed(1999);			// Set your own seed, otherwise, defaults to /dev/random

  // Vertex Distribution (sometimes better to leave at 0)
  //ss->GetGenerator()->SetParameter("vtxflag",TSingleParticleGenerator::FLAT);
  //ss->GetGenerator()->SetParameter("vtxflag",TSingleParticleGenerator::GAUSSIAN);
  //ss->GetGenerator()->SetParameter("zwidth",10);

  se->registerSubsystem(ss);

  //** Here you could put a module to trigger on selected events
  //** For more info, see
  //**  https://www.phenix.bnl.gov/WWW/offline/wikioffline/index.php/PHPythia_Trigger
  //PHPyTrigger* trigger = new PHFvtxTrigger();
  //PHPyTrigger* trigger = new PHPyGammaNccTrigger();
  //trigger->Verbosity(1);
  //se->registerSubsystem(trigger);

  // This is to read in null events (necessary for Fun4All Framework)
  Fun4AllDummyInputManager *in1 = new Fun4AllDummyInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  TString OUTPUT = outputname;
  Fun4AllDstOutputManager *dst_output_mgr  = new Fun4AllDstOutputManager("PHPYTHIA",OUTPUT.Data());
  dst_output_mgr->AddNode("Sync");
  dst_output_mgr->AddNode("PHPythiaHeader");
  dst_output_mgr->AddNode("PHPythia");
  se->registerOutputManager(dst_output_mgr);

  cout<<" PHParticleGen will generate "<<nevents<<" events"<<endl;


  se->run(nevents);  // run over all events
  se->End();
}

