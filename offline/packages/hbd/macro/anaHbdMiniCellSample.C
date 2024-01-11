void anaHbdMiniCellSample(const char *fname1 = "EWGLIST", 
            const char *fname2 = "HBDLIST",
            const char *outFile = "hbd_ewg.root", int nevents=5000)
{
  gSystem->Load("/phenix/data80/ibaf/takao/HbdCodeCheck/offline/packages/hbd/bld/.libs/libhbd.so");
  std::cout << "Load libfun4all.so" << std::endl;
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libfun4all.so");

  std::cout << "anaHBD.C: Get instance of Fun4AllServer" << std::endl;
  Fun4AllServer *se = Fun4AllServer::instance();

  std::cout << "anaHBD.C: Installing MasterRecalibratorManager" << std::endl;
  MasterRecalibratorManager *mr = new MasterRecalibratorManager("MASTERRECALIBRATORMANAGER");
  mr->FillHistos(0);
  se->registerSubsystem(mr);

  //
  // Converter:  HbdMiniCellList -> HbdCellList
  //
  HbdMiniToFullCell *hbdconv = new HbdMiniToFullCell();
  se->registerSubsystem(hbdconv);

  //
  // Here, you insert your clustering module
  //
  HbdClusterizer *hbdclus = new HbdClusterizer();
  se->registerSubsystem(hbdclus);

  //
  // Here, you insert your analysis module
  //
  // YourAnalysisModule  *yourAna = new YourAnalysisModules(); 
  // se->registerSubSystem(yourAna);

  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("EWGin","EWG");
  se->registerInputManager(in1);

  Fun4AllInputManager *in2 = new Fun4AllDstInputManager("HBDin","HBD");
  se->registerInputManager(in2);

  //
  // This is for test.
  // make sure that BlobList is produced properly.
  //
  Fun4AllDstOutputManager *HbdEWGCombined  = new Fun4AllDstOutputManager("HbdEWG",outFile);
  HbdEWGCombined->AddNode("HbdBlobList");

  se->registerOutputManager(HbdEWGCombined);


  //
  // Start of reading input files
  //
  char ewgfile[500];
  char hbdfile[500];
  ifstream runlist1(fname1);
  ifstream runlist2(fname2);
  
  while (runlist1.getline(ewgfile, 500) && runlist2.getline(hbdfile, 500))
    {
      in1->AddFile(ewgfile); // load the filelist into the Input Manager
      in2->AddFile(hbdfile); // load the filelist into the Input Manager
    }

  TStopwatch timer;
  timer.Start();

  se->run(nevents);  // run over nevents (0 == all)
  se->End();

  std::cout << "anaHBD.C: Total running time (Real/CPU) = " << timer.RealTime()
           << " / " << timer.CpuTime() << " sec" << std::endl;

}
