void GenMuidEffic()
		  
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libonlreco.so");
  //gSystem->Load("/phenix/data22/mjkwn/offline/packages/muon_new/mutoo_i_case3/lib/libmutoo_subsysreco.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("MUONFUN4SIM",0);


  //TMuiHVMask::set_disabled(false);
  //TMuiHVMask::set_filename_north("north_90.txt");
  //TMuiHVMask::set_filename_south("south_90.txt");

  MuiooReco* muioo = new MuiooReco();
  MuidEffic* effic = new MuidEffic();
  se->registerSubsystem(muioo);
  se->registerSubsystem(effic);

  // Force the DB initialization
  //
  //se->BeginRun(69522);
  //se->BeginRun(113286);

  //effic->write_const_effic_file("south_100.txt",0,1.0);
  //effic->write_const_effic_file("north_100.txt",1,1.0);
  //return;

  // Run the MUID efficiency analysis
  //

  TChain *muid=0;
  muid = new TChain("muid","muid");
  muid->Add("muid_effic_ntuple_const_90.root");
  effic->do_analysis(muid,0);
  effic->do_analysis(muid,1);

  //effic->write_const_effic_file("south_100.txt",0,1.0);
  //effic->write_const_effic_file("north_100.txt",1,1.0);
  /*
  // Run the SIM efficiency analysis
  //
  TChain *muioo=0;
  muioo = new TChain("muioo","muioo");
  muioo->Add("mutoo_eval_ntuple.root");
  effic->do_sim_analysis(muioo);
  */

  // Translate the files hvgroup files generated in the analysis to standard twopack
  // index format
  //
  //  effic->write_effic_file("nagle_single_muid_ppsouth_trig_hvgroup_eff.dat","sim_south.txt");
  //  effic->write_effic_file("nagle_single_muid_ppnorth_trig_hvgroup_eff.dat","sim_north.txt");
}
