MuonDisplay *muon_display;
Fun4AllServer* se;

void Fun4DSTMuons(int nEvents = 0,                                     // number of events to be processed
		  char *in_dstfile = "mutoo_dst.root",                 // input DST format slow simulator data
		  char *out_dstfile = "new_mutoo_dst.root")            // output DST file name
{
  // CINT can't handle enum
  //
  bool display = false;
  bool use_filter = false;
  
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BFIELDFLAG",3);
  rc->set_IntFlag("MUONFUN4DST",1);
  rc->set_IntFlag("PRINT_MUTOO_PARAMTERS",1);
  
  setMapFileScale(1.0);
  
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  se = Fun4AllServer::instance(); 
  se->Verbosity(0);
    
  ///////////////////////////////////////////
  // Input managers
  ///////////////////////////////////////////
  
  // Arguments are [signal manager name] [dst node name]
  //
  Fun4AllInputManager *dst = new Fun4AllNoSyncDstInputManager("DSTIM","DST");
  se->registerInputManager(dst);

  // Almighty Kluge for analysis of merged file thats missing the run header
  //
  // se->fileopen(dst->Name(),"/phenix/data23/kelly/slow_sim_jpsi/10k_oscar_mutr_selected_jpsi_slowsim_9_29.root");
  // se->fileclose(dst->Name());
  
  se->fileopen(dst->Name(),in_dstfile);
    
  ///////////////////////////////////////////
  // Super Modules
  //////////////////////////////////////////  

  SubsysReco *head = new HeadReco();
  SubsysReco *trig = new TrigReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  SubsysReco *vtx = new VtxReco();
  SubsysReco *mui = new MuiReco();
  SubsysReco *muioo = new MuiooReco();
  SubsysReco *muon_unpack = new MuonUnpackDST();
  SubsysReco *mutoo = new MuonDev();
  SubsysReco *tuples = new MuonAnaTuples();
  SubsysReco *filter = new MuonTrigFilter();  

  // Output DST filter 
  //
  if(use_filter) filter->set_mode(MuonTrigFilter::MUIOO_2D);

  // NDST setup
  //
  //PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
  //SubsysReco *mgw = new MWGReco(MWGcuts);
  
  // Tell Fun4All about the subsystems we want to run
  //
  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(vtx);
  se->registerSubsystem(mui);
  se->registerSubsystem(muon_unpack);
  se->registerSubsystem(muioo);
  se->registerSubsystem(mutoo);
  //  se->registerSubsystem(mgw);    
  se->registerSubsystem(tuples);
  if(use_filter) se->registerSubsystem(filter);

  ///////////////////////////////////////////
  // Output manager
  ///////////////////////////////////////////
  
  Fun4AllIOManager *io  = new Fun4AllIOManager("DSTOUT",out_dstfile);
  if(use_filter) io->AddEventSelector(filter->Name());
  se->registerIOManager(io);
  
  // Ouput dst filter
  //
  if(use_filter) io->AddEventSelector(filter->Name());
  
  io->AddNode("TMutMuiRoad");
  io->AddNode("TMuiMCHit");
  io->AddNode("TMutMCHit");
  io->AddNode("TMutMCTrk");
  io->AddNode("TMuiRoadO");
  io->AddNode("TMuiHitO");
  io->AddNode("TMutHit");
  io->AddNode("TMutClus");
  io->AddNode("TMutGapCoord");
  io->AddNode("TMutCoord");
  io->AddNode("TMutStub");
  io->AddNode("TMutTrk");
  io->AddNode("TMutVtx");
  
  io->AddNode("RunHeader");  
  io->AddNode("EventHeader");  
  io->AddNode("VtxOut");  
  io->AddNode("BbcOut");  
  io->AddNode("BbcRaw");  
  io->AddNode("GlpOut");  
  io->AddNode("dGl1AcptEvtDCM");  
  io->AddNode("TrigLvl1");  

  io->AddNode("dMuiPseudoTriggerOut");  
  io->AddNode("dMuiRoadFkinRel");  
  io->AddNode("dMuoTracks");  
  io->AddNode("dMuoTracksOut");  
  io->AddNode("dMutCalibCathodesOut");  
  io->AddNode("dMutCathodeClusters");  
  io->AddNode("dMutClusterGhitRel");  
  io->AddNode("dMuiClusterRawRel");  
  io->AddNode("dMuiRaw");  
  io->AddNode("dMuiClusters");  
  io->AddNode("dMuiRoadClusterRel");  
  io->AddNode("dMuiRoads");  
  io->AddNode("dMuiRoadRawRel");  
  io->AddNode("dMuoTrackRoadRel");  
  
  // Visualizer
  //
  if(display) {
    muon_display = new MuonDisplay();  
    se->registerSubsystem(muon_display); 
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













