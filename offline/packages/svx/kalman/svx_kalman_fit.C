//
// This is a macro to run the SvxKalmanFitter
// Arguments:
//  1) input DST file containing either SvxSegments or SvxCentralTracks
//  2) output DST file
//  3) debugging TTree file (only produced if compiled with -DDEBUG)
//
void svx_kalman_fit( const char *filein =  "../pisa/singlee_dst.root",
                     const char *fileout = "test.root", 
                     const char *treefileout = "tree.root", 
                     int nEvents = 0 )
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libsimreco.so");
  gSystem->Load("librecal.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";
  gSystem->ListLibraries();
  
  // recoConsts *rc = recoConsts::instance();
  // rc->set_IntFlag("RUNNUMBER", 349369);

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  MasterRecalibratorManager* mrm = new MasterRecalibratorManager;
  se->registerSubsystem(mrm);

  VtxReco* vr = new VtxReco;
  vr->Verbosity(0);
  se->registerSubsystem(vr);

  SvxParManager* svxpar = new SvxParManager;
  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_UsePixelMap(false);
  se->registerSubsystem(svxpar);

  bool use_real_field = true;
  SvxKalmanFitter* svxkf = new SvxKalmanFitter(use_real_field,
                                               treefileout);
  svxkf->SetVerbosity(0);
  svxkf->set_reverse_direction(false);
  svxkf->set_use_central_tracks(true);
  se->registerSubsystem(svxkf);

  // DeathToMemoryHogs *death = new DeathToMemoryHogs();
  // death->event_frequency(1);
  // death->SaveHisto(1);
  // death->memlimit(5000);
  // death->Verbosity(0);
  // se->registerSubsystem(death); 
  
  Fun4AllInputManager *inMan = new Fun4AllNoSyncDstInputManager("DSTIn", "DST");
  inMan->Verbosity(0);
  se->registerInputManager(inMan);

  // Fun4AllInputManager *inMan2 = new Fun4AllPisaInputManager("DSTIn2", "Pisa");
  // inMan2->Verbosity(0);
  // se->registerInputManager(inMan2);

  Fun4AllDstOutputManager *dstout = new Fun4AllDstOutputManager("DSTOut", fileout );
  dstout->AddNode("fkin");
  dstout->AddNode("VtxOut");
  dstout->AddNode("SvxPisaHit");
  dstout->AddNode("SvxGhitList");
  dstout->AddNode("SvxRawhitList");
  dstout->AddNode("SvxClusterList");
  dstout->AddNode("SvxSegmentList");
  dstout->AddNode("SvxTrackList");
  dstout->AddNode("SvxGhitRawhitList");
  dstout->AddNode("SvxRawhitClusterList");
  dstout->AddNode("SvxGhitClusterList");
  se->registerOutputManager(dstout);
  dstout->Print();

  int kEvents = nEvents;
  TFile f(filein);
  int jEvents = T->GetEntries();
  f.Close();

  if(kEvents==0) {
    kEvents = jEvents;
    cout << "\n\n  Event processing will be for " << jEvents << " events total in this input file." << endl;
  }
  else {
    if(kEvents>jEvents) {
      cout << "\n\n  User requests " << kEvents << " input events, but";
      cout << " the input file contains only " << jEvents <<" events." << endl;
      cout << "  Event processing will be for only " << jEvents << " events." << endl;
      cout << "\n  Since in Simulation you are always supposed to know your input conditions perfectly," << endl;
      cout << "  you might want to reconsider what you are doing." << endl << endl;
      kEvents = jEvents;
    }
  }

  inMan->AddFile(filein);
  // inMan2->AddFile("PISAEvent.root");

  //gBenchmark->Start("eventLoop");   // start the timing clock
  se->run(kEvents);                 // process input events
  //gBenchmark->Show("eventLoop");    // complete the timing clock

  se->End();

  PHTimeServer::get()->print_stat();
  delete PHTimeServer::get();

  //delete svxkf;
  delete se;
}
