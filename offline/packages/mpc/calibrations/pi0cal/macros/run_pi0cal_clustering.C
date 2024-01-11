//
// This runs the pi0 calibrations starting from a raw or tower dst
//
void run_pi0cal_clustering(char *outfile, int runnum, int iter, int nevents = 0, int simflag = 0,std::string infile, std::string infile2)
{
  std::cout << std::endl;
  std::cout << "ana_do_clustering.C ... "  << std::endl;
  std::cout << "outfile  = " << outfile  << std::endl;
  std::cout << "infile = " << infile << std::endl;
  std::cout << std::endl;

  //gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libMWG_interface.so");
  //  gSystem->Load("libMpcPi0Cal.so");
  gSystem->Load("/direct/phenix+u/bmeredi2/install/lib/libMpcPi0Cal.so");
  gSystem->Load("libFROG.so");
  FROG fr;

  Fun4AllServer *se = Fun4AllServer::instance();

  se->Verbosity(0);
  
  
  MpcRecalReco *recaltowers;
  recoConsts *rc = recoConsts::instance();
  SubsysReco *mpc_towers;
  if(simflag == 0){
    mpc_towers = new MpcReco("mpc_towers.root");
    rc->set_IntFlag("MPC_RECO_MODE",0x2);	// reading in from raw dst
    se->registerSubsystem(mpc_towers);
  }


  if ( simflag == 1 )
    {
      cout << "Reading in original gains from MpcCal.recal_gains_0" << endl;
      MpcRecalReco *orig_gains = new MpcRecalReco();
      orig_gains->SetCalibFile( "MpcCal.recal_gains_0" );
      se->registerSubsystem( orig_gains );
    }

  // Now put in the determined gains for each tower
  if ( iter>1 )
    {
      TString recal_file = "MpcCal.recal_gains_"; recal_file += (iter-1);
      cout << "Using recal file " << recal_file << endl;
      recaltowers = new MpcRecalReco();
      recaltowers->SetCalibFile( recal_file.Data() );
      se->registerSubsystem(recaltowers);
      recaltowers->Print();
    }
  
  // Now recluster
  SubsysReco *mpc_cluster = new MpcReco("mpc_cluster.root");
  //recoConsts *rc = recoConsts::instance();
  
  rc->set_IntFlag("MPC_RECO_MODE",0x4);	// reading in from tower dst
  se->registerSubsystem(mpc_cluster);

  // Now run the pi0 based calibration
  SubsysReco *ana = new MpcPi0Cal(outfile, iter, runnum);
  se->registerSubsystem(ana);

  // Now add the dst files
  std::cout << infile << std::endl;
  std::cout << "Opening files ...." << std::endl;

  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
  in1->Verbosity(0);
  in1->AddListFile( infile );
  

  // for simulations, read in the monte carlo file
  Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST");
  se->registerInputManager(in2);
  in2->Verbosity(0);
  if ( simflag == 1 )
    {
      // Get the pythia formatted file
      TString py_infile = infile;
      py_infile.ReplaceAll("e_","py_e_");
      in2->AddListFile(py_infile.Data());
    }
  else
    {
      // Read in file with PHGlobal
      in2->AddListFile(infile2);
    }
  
  se->run(nevents);
  
  se->End();

}

