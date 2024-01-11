// $Id: Fun4Display_sim.C,v 1.2 2004/02/19 16:09:56 hpereira Exp $

Fun4AllServer* se;
Muon3DDisplay *muon_display;

void Fun4Display_sim(
  int nEvents = 1000,                   // number of events to be processed
  char *signal_file = "jpsi_sim.root",  // input DST format slow simulator data
)
{
  
  // libraries are loaded in the rootlogon file
  bool pure_monte_carlo = true;

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("BFIELDFLAG",3);
  rc->set_IntFlag("SIMULATIONFLAG",2);
  rc->set_IntFlag("MUONFUN4SIM",1);
  
  // define default run and time stamp
  int default_run = 79641;
  rc->set_IntFlag("RUNNUMBER",default_run);

  setMapFileScale(1.0);


  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  se = Fun4AllServer::instance(); 
  se->Verbosity(0);
    
  ///////////////////////////////////////////
  // Input managers
  ///////////////////////////////////////////
  
  Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager("SIGNAL_IM","SIGNAL");
  se->registerInputManager(signal);
  se->fileopen(signal->Name(),signal_file);  

  if(!pure_monte_carlo) {
    Fun4AllInputManager *background = new Fun4AllNoSyncDstInputManager("BACKGROUND_IM","BACKGROUND");
    se->registerInputManager(background);
    se->fileopen(background->Name(),background_file);
  }
  
  ///////////////////////////////////////////
  // Super Modules
  //////////////////////////////////////////
  
  SubsysReco *muon_unpack = new MuonUnpackSim();  
  SubsysReco *mutoo_reco = new MutooReco();   
  SubsysReco *muioo_reco = new MuiooReco();   
  muon_display = new Muon3DDisplay();  
  se->registerSubsystem(muon_unpack); 

  se->registerSubsystem(mutoo_reco); 
  se->registerSubsystem(muioo_reco);   
  se->registerSubsystem(muon_display); 

  ///////////////////////////////////////////
  // Set the Mode
  //////////////////////////////////////////
  if(pure_monte_carlo) muon_unpack->SetMode(MuonUnpackSim::MC_SIGNAL_NO_BG);
  else muon_unpack->SetMode(MuonUnpackSim::MC_SIGNAL_MC_BG);
  se->run(1);
  cout << "Visualize Commands" << endl;
  cout << "usage: muon_display commands:" << endl;
  muon_display->help( "muon_display" );
  cout << "--------------------------------------------------------------------------------" << endl;
  cout << "To get next event type: se->run(1)" << endl;

}
