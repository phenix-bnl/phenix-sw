MuonDisplay *muon_display;
Fun4AllServer* se;

void Fun4PRDFMuons(char *prdffile = "data.prdf", 
		   const char *dstout = "mutoo_dst.root",
		   ULong_t nEvents=10)
{
  bool display=false;
  bool use_filter=true;

  se = Fun4AllServer::instance();  
  recoConsts *rc = recoConsts::instance();
  
  rc->set_IntFlag("PPFLAG", 0);
  rc->set_IntFlag("BFIELDFLAG", 1);
  rc->set_IntFlag("SIMULATIONFLAG", 0);
  rc->set_IntFlag("EMBEDFLAG", 0);
  rc->set_IntFlag("GEOMFLAG", 1);
  rc->set_IntFlag("EVALUATIONFLAG", 0);

  setMapFileFlag(3);  
  setMapFileScale(1.0); 

  SubsysReco *head = new HeadReco();
  SubsysReco *trig = new TrigReco();
  SubsysReco *bbc = new BbcReco();
  SubsysReco *zdc = new ZdcReco();
  SubsysReco *vtx = new VtxReco();
  SubsysReco *mui = new MuiReco();
  SubsysReco* unpack = new MuonUnpackPRDF();
  SubsysReco *muioo = new MuiooReco();
  SubsysReco *mutoo = new MuonDev();
  SubsysReco *tuples = new MuonAnaTuples();
  SubsysReco *filter = new MuonTrigFilter();  
  
  // Pre-filter
  //
  if(use_filter) { 
    filter->set_mode(MuonTrigFilter::MUIOO_2D);
    filter->set_action(MuonTrigFilter::ABORT_EVENT);
  }

  // NDST setup
  //
  PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
  SubsysReco *mgw = new MWGReco(MWGcuts);
  
  // Tell Fun4All about the subsystems we want to run
  //
  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(vtx);

  se->registerSubsystem(unpack);
  se->registerSubsystem(muioo);
  if(use_filter) se->registerSubsystem(filter);
  se->registerSubsystem(mutoo);
  se->registerSubsystem(mui);
  se->registerSubsystem(mgw);    
  se->registerSubsystem(tuples);

  
  se->Verbosity(0);
  
  // Visualizer
  //
  if(display) {
    muon_display = new MuonDisplay();  
    se->registerSubsystem(muon_display); 
  }
  
  // DST output manager node.
  //
  Fun4AllIOManager *io = new Fun4AllIOManager("DSTOUT", dstout);
  
  // Ouput dst filter
  //
  if(use_filter) io->AddEventSelector(filter->Name());
  
  // NDST output manager node.
  //
  char *mwgfile = "MWG.root";
  Fun4AllIOManager *io_mwg = new Fun4AllIOManager("MWGOUT",mwgfile);
  
  io->AddNode("TMuiHitO");
  io->AddNode("TMuiClusterO");
  io->AddNode("TMuiRoadO");
  io->AddNode("TMuiPseudoBLTO");
  io->AddNode("TMutHit");  
  io->AddNode("TMutClus");  
  io->AddNode("TMutCoord");  
  io->AddNode("TMutGapCoord");  
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
  
  io_mwg->AddNode("PHGlobal");
  io_mwg->AddNode("PHMuoTracksOO");
  
  se->registerIOManager(io);
  se->registerIOManager(io_mwg);
  
  pfileopen(prdffile);
  
  // Run the analysis
  //
  if(display) {
    prun(1);
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
    cout << "To get next event type: prun(1)" << endl;
  } else {
    prun(nEvents);
    se->EndRun();
    cout << "Completed reconstruction." << endl;
  }

}

