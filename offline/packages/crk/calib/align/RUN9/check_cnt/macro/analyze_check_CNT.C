void analyze_check_CNT(
		       char *inFileList = "inout.txt",
		       char *OutFile = "output.root"
		       )
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");

  gSystem->Load("../install/lib/libcheck_cnt.so");

 /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...

  //  MasterRecalibratorManager *mr = new MasterRecalibratorManager();
  //  se->registerSubsystem(mr);

  SubsysReco *example = new check_cnt(OutFile);
  example->Verbosity(0);

  se->registerSubsystem(example);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  ////////////////////////////////////////////////////////////////

  char dstfile1[500];

  ifstream runlist1(inFileList);

  while (runlist1.getline(dstfile1, 500))
    {
      cout << dstfile1 << endl;
      in1->AddFile(dstfile1); // load the filelist into the Input Manager
    }

  cout << " RUN " <<endl;
  se->run(0);  // run over all events
  //    se->run(1000);  // run over 1000 events
  se->End();
}
