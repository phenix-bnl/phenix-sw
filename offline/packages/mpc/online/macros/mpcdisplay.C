void mpcdisplay(char *inFileList = "dst_mpc.list", const int nevents = 0)
{
  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  gSystem->Load("libmpc.so");
  gSystem->Load("libmpconline.so");

  gStyle->SetOptStat(0);

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("MPC_RECO_MODE",0x6);

  SubsysReco *mpcreco = new MpcReco("MPCRECO");
  se->registerSubsystem( mpcreco );

  SubsysReco *display = new MpcSimpleEvtDisp("MPCDISPLAY");
  display->Verbosity(1);

  se->registerSubsystem(display);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  in1->AddListFile( inFileList ); // load the filelist into the Input Manager

/*
  TString inFileList2 = inFileList;
  inFileList2.ReplaceAll( "dst_mpc", "ewg" );
  Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2", "DST");
  se->registerInputManager(in2);
  in2->AddListFile( inFileList2.Data() );
*/

  se->run( nevents );  // run over all events
  se->End();

}

