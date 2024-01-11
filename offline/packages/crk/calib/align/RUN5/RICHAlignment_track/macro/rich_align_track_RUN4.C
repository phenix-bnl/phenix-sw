void rich_align_track_RUN4(
			   char *inFileList1 = "RICH_Alignment_RUN4_input_CNT.txt", 
			   char *inFileList2 = "RICH_Alignment_RUN4_input_DST_CRK.txt", 
			   char *OutFile = "RICH_Alignment_RUN4_output.root")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("liblvl2.so");
  gSystem->Load("libaggro.so");
  gSystem->Load("libMWG_interface.so");

  gSystem->Load("librichalignment_track.so");

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  SubsysReco *example = new RICHAlignment_track(OutFile);
  example->Verbosity(0);

  se->registerSubsystem(example);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);
  Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2", "DST");
  se->registerInputManager(in2);
  /////////////////////////////////////////////////////////////////
  //  Branch Selection...
  //  NOTE:  This is a dangerous option.
  //         --Selecting only the needed branches speeds up code slot.
  //         --However, if you forget even one branch you are in trouble.
  //         (you will probably see that the emc burner does not work, since it's vars
  //          are not enabled)
  //  During organized analysis passes we will read all branches...slow but safe.  
  //se->BranchSelect("DSTin1", "Central.*", 0);
  //se->BranchSelect("DSTin1", "Central.mom", 1);
  //se->BranchSelect("DSTin1", "Central.phi0", 1);
  //se->BranchSelect("DSTin1", "Central.the0", 1);

  ////////////////////////////////////////////////////////////////

  char dstfile1[500];
  char dstfile2[500];
  
  ifstream runlist1(inFileList1);
  ifstream runlist2(inFileList2);

  while (runlist1.getline(dstfile1, 500))   
    {
      in1->AddFile(dstfile1); // load the filelist into the Input Manager
      cout << "DSTin1 " << dstfile1 << endl;
    }
  while (runlist2.getline(dstfile2, 500)) 
    {
      in2->AddFile(dstfile2); // load the filelist into the Input Manager
      cout << "DSTin2 " << dstfile2 << endl;
    }
  
  cout << " RUN " <<endl;
  se->run(50000);  // run over all events
  se->End();
}
