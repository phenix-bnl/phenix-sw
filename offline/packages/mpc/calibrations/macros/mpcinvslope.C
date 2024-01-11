//
// study the mpc inverse slope
//
void mpcinvslope(unsigned int nevents = 0, const char *fname = "DST_MPC.root")
{
  //gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  //gSystem->Load("libfun4all.so");	// framework only
  gSystem->Load("libmpc.so");
  gSystem->Load("libmpccalib.so");

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  //MasterRecalibratorManager *mr = new MasterRecalibratorManager();
  //se->registerSubsystem(mr);

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("MPC_RECO_MODE",0x2);

  SubsysReco *mpcreco = new MpcReco("MPCRECO");
  se->registerSubsystem(mpcreco);

  SubsysReco *mpcslope = new MpcInvSlope("MPCSLOPE");
  se->registerSubsystem(mpcslope);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  //in1->AddEventSelector("MPCONLINE");

  /////////////////////////////////////////////////////////////////
  // Now add your file lists
  //TString mpc_list = "dst.list";
  //in1->AddListFile( mpc_list.Data() );
  in1->AddFile( fname );

  se->run(nevents);  // run over all events
  se->End();
}
