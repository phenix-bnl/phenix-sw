//
// this macro reads existing PHPythia output, 
// shifts event vertices according to "vtxfilename" content,
// and writes the result to an output file.
// 
void shift_vertex(
  const int nevents = 10000, 
  //const char *inputname = "electrons_000.root",
  const char *inputname = "positrons_009.root",
  const char *vtxfilename = "../reco/vtxlist_347129_0009_0-20cent.dat",
  //const char *outputname = "elec_000_shifted_347129_0000_0-20cent.root"
  const char *outputname = "posi_009_shifted_347129_0009_0-20cent.root"
  )
{
  //gSystem->Load("libfun4allfuncs.so");	// framework only
  gSystem->Load("libfun4all.so");	// framework + reco modules
  //gSystem->Load("/phenix/u/workarea/lebedev/chi_c_run8dAu/event_gen/src/PHPythia/install/lib/libPHPythiaEventGen.so");
  //gSystem->Load("/phenix/u/workarea/lebedev/chi_c_run8dAu/event_gen/src/PHPythia/install/lib/libPHPythia.so");
  //gSystem->Load("/phenix/u/workarea/lebedev/chi_c_run8dAu/event_gen/src/PHPythia/install/lib/libPHPyTrigger.so");
  //gSystem->Load("/phenix/u/workarea/lebedev/chi_c_run8dAu/event_gen/src/PHPythia/install/lib/libPHPyParticleSelect.so");
  //gSystem->Load("libPHPythiaEventGen.so");
  gSystem->Load("libPHPythia.so");
  //gSystem->Load("libPHPyTrigger.so");
  //gSystem->Load("libPHPyParticleSelect.so");
  gSystem->Load("libsimreco.so");	// framework + reco modules

//  recoConsts *rc = recoConsts::instance();
//  rc->set_IntFlag("RUNNUMBER",0);

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(1);

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
//  SubsysReco *sync = new SyncSimreco();
//  se->registerSubsystem(sync);

  PHPyVertexShift *vshift = new PHPyVertexShift();
  vshift->SetVtxFile(vtxfilename); 
  vshift->Verbosity(1);
  se->registerSubsystem(vshift);

//  //** A dummy (null) input is needed for the Fun4All framework
//  Fun4AllDummyInputManager *in1 = new Fun4AllDummyInputManager("DSTin1", "DST");
//  se->registerInputManager(in1);

  Fun4AllDstInputManager *in = new Fun4AllDstInputManager("inputPHPythia","DST");
  se->registerInputManager(in);
  //in->AddListFile(inFileList); // load the filelist into the Input Manager
  in->AddFile(inputname);
  in->Verbosity(1);


  // DST output manager
  TString OUTPUT = outputname;
  Fun4AllDstOutputManager *dst_output_mgr  = new Fun4AllDstOutputManager("PHPYTHIA",OUTPUT.Data());
  dst_output_mgr->AddNode("Sync");
  dst_output_mgr->AddNode("PHPythiaHeader");
  dst_output_mgr->AddNode("PHPythia");
  dst_output_mgr->Verbosity(1);
  se->registerOutputManager(dst_output_mgr);

  // run over all events
  se->run(nevents);  
  se->End();

}

