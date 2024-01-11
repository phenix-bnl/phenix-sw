void Run_Run16dAuPi0Photon_BBC2(const char *outFile = "dummy.root")
{

  // // gSystem->Setenv("ODBCINI",","/opt/phenix/etc/odbc.ini.test");

  // gSystem->Load("libuspin.so");

  gSystem->Load("libRun16dAuPi0Photon.so");
  
  Fun4AllServer *se = Fun4AllServer::instance();

  //  recoConsts *rc = recoConsts::instance();
  //  rc->set_IntFlag("EMCDEADMAPVER",10);
  
  recoConsts *reco_consts =  recoConsts::instance();
  reco_consts->set_IntFlag("EMCNEW_DEBUG", 0);      // set debugging verbosity level
  reco_consts->set_IntFlag("EMCTP_IS_PP", 0);  /// is it a p+p pruduction? set 0 for d+Au, 1 for p+p
  reco_consts->set_IntFlag("EMCTP_IS_ERT", 0); /// is it a ERT pruduction? set 0 for MinBias, 1 for ERT
  reco_consts->set_IntFlag("TRIG_SEL", 4); /// 0- BBC, 1- BBCnovtx, 2- BBCnarrow, 4- central

  reco_consts->set_FloatFlag("EMC_NTUPLE_PTCUT", 5);
  se->registerSubsystem( new RecalWarnmap() );
  //se->registerSubsystem( new RecalEMCalTOF() );  
  se->registerSubsystem( new Run16dAuPi0Photon(outFile) );
  
}

void InputData(vector<string> &indata)
{
  indata.push_back("CNT");
  indata.push_back("DST_EVE");
  return;
}

