void analyze_gain_calib(char *inFileList = "file_EWG_MinBias_run7AuAu_200GeV.list_1",char *inFileList2 = "file_DST_HBD_MinBias_run7AuAu_200GeV.list_1", char *outFile = "Calibrator.root")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libCNT.so");
  gSystem->Load("liblvl2.so");
   gSystem->Load("libepairAnalyzer.so");
   gSystem->Load("libhbd.so");
   gSystem->Load("libhbdhnscluster.so");
   gSystem->Load("librecal.so");
   gSystem->Load("libhbdcalibrator.so");


  /////////////////////////////////////////////////////////////////
  // Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ////////////////
  // Recalibrator
  MasterRecalibratorManager *mr = new MasterRecalibratorManager();
  //se->registerSubsystem(mr);


  // Converter:  HbdMiniCellList -> HbdCellList
  //only for DST_HBD
  HbdMiniToFullCell *hbdconv = new HbdMiniToFullCell();
  se->registerSubsystem(hbdconv);

  HbdCalibrator *hbdcalib = new HbdCalibrator();
  //hbdcalib->Verbosity(0);
  // se->registerSubsystem(hbdcalib);

  // GainAnalyzer
  //SubsysReco *gain = new GainAnalyzer(outFile);//for official one
  //SubsysReco *gain = new GainAnalyzer();
  //SubsysReco *gain = new GainAnalyzer(256450);
  SubsysReco *gain = new GainAnalyzer(234572); 
  se->registerSubsystem(gain);

  SubsysReco *clusterizer = new HbdHnSClusterizer();
  //se->registerSubsystem(clusterizer);


  /////////////////////////////////////////////////////////////////
  // Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1","EWG");
  in1->Verbosity(0);
  Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2","HBD");//EWG
  in2->Verbosity(0);

 
  se->registerInputManager(in1);
  se->registerInputManager(in2);

  ///////////////
  // while thru file list and run
  char dstfile[500];
  ifstream runlist(inFileList);

  TStopwatch timer;

  while (runlist.getline(dstfile, 500))
    {
      in1->AddFile(dstfile); // load the filelist into the Input Manager
    }

///////////////
  // while thru file list and run
  
  char dstfilehbd[500];
  ifstream runlisthbd(inFileList2);

  while (runlisthbd.getline(dstfilehbd, 500))
    {
      in2->AddFile(dstfilehbd); // load the filelist into the Input Manager
    }

  


  //timer.Start();

  se->run(1000);//10000  // run over all events (0)
  se->End();
  // se->dumpHistos(outFile);

  //timer.Stop();
  //cout << "took real time: " << timer.RealTime() << " took cpu time: " << timer.CpuTime() <<endl;
}
