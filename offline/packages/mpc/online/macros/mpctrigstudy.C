
void mpctrigstudy(const char *runnumber = "", const int nevt=0)
{
  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  //gSystem->Load("libfun4all.so");	// framework only
  gSystem->Load("libmpc.so");
  gSystem->Load("libmpconline.so");

  gStyle->SetOptStat(0);

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  //SubsysReco *vtx = new VtxReco();
  //se->registerSubsystem(vtx);

  //SubsysReco *global = new GlobalReco();
  //se->registerSubsystem(global);

  TString name = "mpctrigstudy"; name += runnumber; name += ".root";
  SubsysReco *example = new MpcTriggerStudy( name.Data() );
  example->Verbosity(1);

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
  //  OK, now loop over all the input files...
/*
  char dstfile[500];
  ifstream runlist(inFileList);

  while (runlist.getline(dstfile, 500)) // getline prevents overflow beyond 500 chars
    {
      in1->AddFile(dstfile); // load the filelist into the Input Manager
    }
*/

  in1->AddListFile( "f.list" );
  cout << "Adding f.list" << endl;
  in2->AddListFile( "eve_f.list" );
  cout << "Adding eve_f.list" << endl;

  se->run(nevt);  // run over all events
  se->End();
}

