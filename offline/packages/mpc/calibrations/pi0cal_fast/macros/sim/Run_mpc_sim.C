void Run_mpc_sim(char* runnum, char* outputdir, unsigned int nevents = 0,
		 char* mpc_list = 0, char* all_list = 0, char* pwg_list = 0, 
		 char* cnt_list = 0, char* mwg_list = 0 )
{

  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  gSystem->Load("librecal.so");
  gSystem->Load("libMWG_interface.so");
  gSystem->Load("libmpc.so");
  gSystem->Load("libmpcpi0ttree.so");
  gSystem->Load("libanaCalibSimTowers.so");
  gSystem->Load("libanaMpcEmbedPrep.so");
  

  cout << "hello1\n";  
  gStyle->SetOptStat(0);
  ////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  
  //we have to do mpc clustering on the fly right now
  recoConsts *rc = recoConsts::instance();
  
  
  // Set this to 0x6 to read from the raw dst,
  // or 0x4 to read from the calibrated tower dst
  
  SubsysReco *global  = new GlobalReco();
  se->registerSubsystem(global);
  
  SubsysReco* calib_towers = new CalibSimTowers(2,0.04,0.05,4);  //5% calib uncertainty
  
  se->registerSubsystem(calib_towers,"TOP");//should be all we ne
  
  rc->set_IntFlag("MPC_RECO_MODE",0x4);
  
  rc->set_IntFlag("MPC_CLUSTER_ALG",1);  
  rc->set_IntFlag("MPC_CG_FLAG",2);  
  rc->set_IntFlag("RUNNUMBER",257511);
  

  cout << "hello2\n";  
  SubsysReco *mpc_cluster = new MpcReco("mpc_cluster.root");

  se->registerSubsystem(mpc_cluster);
  
  
  
  cout << "hello3\n";  
  Fun4AllDstInputManager *inmpc = new Fun4AllDstInputManager("DSTmpc", "DST");
  Fun4AllDstInputManager *inall = new Fun4AllDstInputManager("DSTall", "DST");
  Fun4AllDstInputManager *incnt = new Fun4AllDstInputManager("DSTcnt", "DST");
  Fun4AllDstInputManager *inpwg = new Fun4AllDstInputManager("DSTpwg", "DST");
  Fun4AllDstInputManager *inmwg = new Fun4AllDstInputManager("DSTmwg", "DST");

  //  Reconstruction Modules...
  
  cout << "hello4\n";  
  //  char* inDeadtowerList = "deadmap.txt";
  TString outfn = "condor_output/pi0_sim_"; outfn+=runnum; outfn+=".root";
  
  cout << "hello5\n";  
  SubsysReco *example;
  cout << "hello5\n";  
  example = new mpcPi0TTree(outfn.Data(),"mpcwm.txt");
  cout << "hello5\n";  
  
  cout << "hello6\n";  
  example->Verbosity(1);
  
  cout << "hello7\n";  
  se->registerSubsystem(example);
  cout << "hello\n";  
    
  ////////////////////////////////////////////////////////////////
  //  Input Managers...

  //cout << mpc_list << ", " << all_list << endl;
  if(mpc_list != 0)   se->registerInputManager(inmpc);
  if(all_list != 0)   se->registerInputManager(inall);
  if(pwg_list != 0)   se->registerInputManager(inpwg);
  if(cnt_list != 0)   se->registerInputManager(incnt);
  if(mwg_list != 0)   se->registerInputManager(inmwg);  

  
  if(mpc_list != 0) inmpc->AddListFile(mpc_list);
  if(all_list != 0) inall->AddListFile(all_list);
  if(pwg_list != 0) inpwg->AddListFile(pwg_list);
  if(cnt_list != 0) incnt->AddListFile(cnt_list);
  if(mwg_list != 0) inmwg->AddListFile(mwg_list);  

  
  cout << "hello\n";  
  se->run(nevents);  // run over all events
  
  cout << "hello\n";  
  se->End();
  
}
