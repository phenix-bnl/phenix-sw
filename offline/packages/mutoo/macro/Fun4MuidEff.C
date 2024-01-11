MuonDisplay *muon_display;
Fun4AllServer* se;

void Fun4MuidEff(int nEvents     = -1,                                // number of events to be processed
		 char *signal_file = "signal.root",                   // input DST format slow simulator data
		 char *background_file = "background.root",           // input DST 
		 char *dstfile   = "mutoo_dst.root")                  // output DST file name
{
  // CINT can't handle enum
  //
  bool pure_monte_carlo = true;
  bool display = false;

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BFIELDFLAG",3);
  rc->set_IntFlag("MUONFUN4SIM",1);
  setMapFileScale(1.0);

  TMuiHVMask::set_disabled(false);
  TMuiHVMask::set_filename_north("north.txt");
  TMuiHVMask::set_filename_south("south.txt");

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  se = Fun4AllServer::instance(); 
  se->Verbosity(0);
  
  ///////////////////////////////////////////
  // Output manager
  ///////////////////////////////////////////
  Fun4AllIOManager *dstManager  = new Fun4AllIOManager("DSTOUT",dstfile);
  se->registerIOManager(dstManager);
  dstManager->AddNode("TMutMuiRoad");
  dstManager->AddNode("TMuiMCHit");
  dstManager->AddNode("TMutMCHit");
  dstManager->AddNode("TMutMCTrk");
  dstManager->AddNode("TMuiRoadO");
  dstManager->AddNode("TMutHit");
  dstManager->AddNode("TMutClus");
  dstManager->AddNode("TMutGapCoord");
  dstManager->AddNode("TMutCoord");
  dstManager->AddNode("TMutStub");
  dstManager->AddNode("TMutTrk");
  dstManager->AddNode("TMutVtx");
  
  ///////////////////////////////////////////
  // Input managers
  ///////////////////////////////////////////
  
  // Arguments are [signal manager name] [dst node name]
  //
  Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager("SIGNAL_IM","SIGNAL");
  se->registerInputManager(signal);
  se->fileopen(signal->Name(),signal_file);  
  
  // Don't open the background file if in pure Monte-Carlo mode
  //
  if(!pure_monte_carlo) {
    Fun4AllInputManager *background = new Fun4AllNoSyncDstInputManager("BACKGROUND_IM","BACKGROUND");
    se->registerInputManager(background);
    se->fileopen(background->Name(),background_file);
  }

  ///////////////////////////////////////////
  // Super Modules
  //////////////////////////////////////////
  
  SubsysReco *muon_unpack = new MuonUnpackSim();  
  se->registerSubsystem(muon_unpack); 
  SubsysReco *muioo_reco = new MuiooReco();   
  se->registerSubsystem(muioo_reco); 
  SubsysReco *mutoo_reco = new MutooReco();   
  se->registerSubsystem(mutoo_reco); 
  SubsysReco *muon_eval = new MuonEval();   
  se->registerSubsystem(muon_eval); 
  SubsysReco *muon_effic = new MuidEffic();   
  se->registerSubsystem(muon_effic); 

  // Visualizer
  //
  if(display) {
    muon_display = new MuonDisplay();  
    se->registerSubsystem(muon_display); 
  }

  ///////////////////////////////////////////
  // Set the Mode
  //////////////////////////////////////////
  if(pure_monte_carlo) {
    muon_unpack->SetMode(MuonUnpackSim::MC_SIGNAL_NO_BG);
  } else {
    muon_unpack->SetMode(MuonUnpackSim::MC_SIGNAL_MC_BG);
  }
  
  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  if(display) {
    se->run(1);
    cout << "Visualize Commands" << endl;
    cout << "usage: muon_display->draw_octant(arm,octant,station)" << endl;
    cout << "usage: muon_display->draw_octant(arm,octant)" << endl;
    cout << "usage: muon_display->draw_octant(arm)" << endl;
    cout << "usage: muon_display->draw_side(arm,octant,station)" << endl;
    cout << "usage: muon_display->draw_side(arm,octant)" << endl;
    cout << "usage: muon_display->draw_plane(arm,station)" << endl;
    cout << "--------------------------------------------------------------------------------" << endl;
    cout << "Dump Commands" << endl;
    cout << "usage: muon_display->dump_hit()" << endl;
    cout << "usage: muon_display->dump_trk()" << endl;
    cout << "etc...." << endl;
    cout << "--------------------------------------------------------------------------------" << endl;
    cout << "To get next event type: se->run(1)" << endl;
  } else {
    se->run(nEvents);
    se->EndRun();
    cout << "Completed reconstruction." << endl;
  }
}













