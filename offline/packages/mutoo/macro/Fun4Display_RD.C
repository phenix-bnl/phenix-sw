
MuonDisplay *muon_display;
Fun4AllServer* se;

/*! 
  dst analysis loop for real data dst
  using mutoo for reconstruction
  and mutoo_hack supermodule for evaluation
*/
void Fun4Display_RD(
  int nEvents = 200,             
  char *signal_file = "jpsi_rd.root"
)           
{

  bool pure_monte_carlo = true;

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BFIELDFLAG",3);
  setMapFileScale(1.0);
  
  // other switches
  rc->set_IntFlag("MUTOO_L2_VTX",1);
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);
  
  // define default run and time stamp
  int default_run = -79641;
  rc->set_IntFlag("RUNNUMBER",default_run);

  // disable HV mask lookup
  TMuiHVMask::set_disabled( true );
		

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  se = Fun4AllServer::instance(); 
  se->Verbosity(0);
  
  
  ///////////////////////////////////////////
  // Input managers
  ///////////////////////////////////////////
  
  // Arguments are [signal manager name] [dst node name]
  Fun4AllInputManager *signal = new Fun4AllDstInputManager("DST","DST");
  se->registerInputManager(signal);
  se->fileopen(signal->Name(),signal_file);  

  ///////////////////////////////////////////
  // Super Modules
  //////////////////////////////////////////
  
  SubsysReco *muon_unpack = new MuonUnpackDST();
  SubsysReco *muioo_reco  = new MuiooReco();   
  SubsysReco *mutoo_reco  = new MutooReco();   
  muon_display = new MuonDisplay();  
  
  se->registerSubsystem(muon_unpack); 
  se->registerSubsystem(muioo_reco); 
  se->registerSubsystem(mutoo_reco); 
  se->registerSubsystem(muon_display); 

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
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

}













