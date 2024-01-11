void mpcmip(char *inMpcFileList,char *inBbcFileList, unsigned int nevents = 0)
{
  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  //gSystem->Load("libfun4all.so");	// framework only
  //gSystem->Load("libmpc.so");
  gSystem->Load("liblpc.so");
  gSystem->Load("libmpconline.so");

  gStyle->SetOptStat(0);

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  SubsysReco *mpcmip = new MpcMip("mpcmip_hist.root","mpcmip_tree.root");  // arguments are output filename
  se->registerSubsystem(mpcmip);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);
  Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2", "DST");
  se->registerInputManager(in2);

  /////////////////////////////////////////////////////////////////
  // Now add your files
  in1->AddListFile(inMpcFileList);
  in2->AddListFile(inBbcFileList);

  se->run(nevents);  // run over all events
  se->End();
}
