void gen_twopack_eff(const char * input_file="HVchainbase_eff.txt", char * output_file="twopackbase_eff.txt")
{

  //This macro is converting HVchain base tube efficiency file to twopack base efficieny file
  //input should be the hv chain base output from do_analysis() function
  gSystem->Load("libfun4all.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("MUONFUN4SIM",0);
  rc->set_IntFlag("MUONRECO",1); 
  rc->set_IntFlag("MUTRSTRIP",0);
  rc->set_IntFlag("MUONFUN4DST",0);

  rc->set_IntFlag("MUID",1);
  rc->set_IntFlag("MUTR",0);

  PHTimeStamp t;
  t.set(2005, 6, 30, 12, 0, 0);
  se->BeginRunTimeStamp(t);

  SubsysReco *muioo   = new MuiooReco();
  SubsysReco* unpack = new MuonUnpackPRDF();

  se->registerSubsystem(unpack);
  se->registerSubsystem(muioo);


  MuidEffic* effic = new MuidEffic();
  effic->set_muid_only(true);
  effic->set_include_err(false);
  se->registerSubsystem(effic);

  pfileopen("/phenix/data25/MUON_GROUP_PROJECTS/run4AuAu_prdf/115227/EVENTDATAxxx_P01-0000115227-0000.PRDFF");
  pidentify(1);
  prun(1); // run one event

  se->End();

  cout << " running done - now let's done the file translation" << endl;
  effic->write_effic_file(input_file,output_file);
}
