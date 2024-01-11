void analyze_check_Crk(
		       char *inFileList = "input.txt",
		       char *OutFile = "output.root"
		       )
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("../install/lib/libcheck_crk.so");

 /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  SubsysReco *example = new check_crk(OutFile);
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
      in1->AddFile(dstfile1); // load the filelist into the Input Manager
    }

  cout << " RUN " <<endl;
  se->run(0);  // run over all events
  se->End();
}
