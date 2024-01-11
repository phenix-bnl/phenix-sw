void ReflexiveMuidEffic()
		  
{
  // Muid Plane efficiency reflexive test
  // 
  // JN algorithm input from muid_effic_ntuple.root
  // MC algorithm input from mutoo_eval_ntuple.root
  // Efficiency input from TMuiHVMask input files

  // Note the reflexive test will only produce sensible
  // results is the 3 above inputs are consistent. That
  // is the two root files listed above should be generated
  // with the tube efficiency files used to initialize
  // TMuiHVMask below.
 
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("MUONFUN4SIM",1);

  TMuiHVMask::set_disabled(false);
  TMuiHVMask::set_filename_north("north.txt");
  TMuiHVMask::set_filename_south("south.txt");

  MuiooReco* muioo = new MuiooReco();
  MuidEffic* effic = new MuidEffic();
  se->registerSubsystem(muioo);
  se->registerSubsystem(effic);

  // Force the DB initialization
  //
  se->BeginRun(69522);

  unsigned short arm=0;

  // Run the MUID efficiency analysis
  //
  TChain *muid=0;
  muid = new TChain("muid","muid");
  muid->Add("muid_effic_ntuple.root");
  effic->do_analysis(muid, arm);

  // Run the SIM efficiency analysis
  //
  TChain *muid_sim=0;
  muid_sim = new TChain("muioo","muioo");
  muid_sim->Add("mutoo_eval_ntuple.root");
  effic->do_sim_analysis(muid_sim, arm);

  // Write the reflexive test ntuples
  //
  effic->write_reflexive(arm);
  std::cout << "ReflexiveMuidEffic -- complete" << std::endl;

}




