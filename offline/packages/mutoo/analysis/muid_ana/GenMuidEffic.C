void GenMuidEffic(int ncut)
		  
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libonlreco.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("MUONFUN4SIM",0);

  MuiooReco* muioo = new MuiooReco();
  MuidEffic* effic = new MuidEffic();
  se->registerSubsystem(muioo);
  se->registerSubsystem(effic);

  // Force the DB initialization
  se->BeginRun(113286);

  // Run the MUID efficiency analysis
  //

  TChain *muid=0;
  muid = new TChain("muid","muid");
  muid->Add("muid_effic_ntuple_const_90.root");
  effic->set_cutselect(ncut);
  effic->do_analysis(muid,0);
  effic->do_analysis(muid,1);


  // Translate the files hvgroup files generated in the analysis to standard twopack
  // index format
  //
  //  effic->write_effic_file("nagle_single_muid_ppsouth_trig_hvgroup_eff.dat","sim_south.txt");
  //  effic->write_effic_file("nagle_single_muid_ppnorth_trig_hvgroup_eff.dat","sim_north.txt");
}




