void analyze(char *out)
{
  cout << "Starting job" << endl;//debug

  gSystem->Load("libfun4all.so");
  gSystem->Load("libSubsysReco.so");
  gSystem->Load("libfun4allfuncs.so");  
  gSystem->Load("libtrigger.so");  
  gSystem->Load("libspin.so");  
  gSystem->Load("liblpc.so"); 
  //give proper path name for library
  gSystem->Load("/phenix/spin/data78/phnxsp02/okada/work/20110214/install/lib/libVernier.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  //se->Verbosity(1);
  cout << "creating Fun4All server." << endl;

  SubsysReco *vernscan = new Vernier(out);
  se->registerSubsystem(vernscan);
  cout << "registering module as a subsystem."  << endl;

  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);
  cout << "registering input manager." << endl;
  cout << " " << endl;

  //======================================================
  std::string filename;
  //make a list of dst's and give proper path name
  ifstream inputlist("./dstlist.txt", ios :: in);
  if(inputlist.is_open())
    {
      inputlist >> filename;
      while(!inputlist.eof())
	{
	  in1->AddFile(filename.c_str());
	  cout << "adding file: " << filename.c_str() << endl;
          inputlist >> filename;
	}
      se->run(0);
      se->End();
      
      cout << "job done." << endl;//debug
    }
  else 
    cout << "could not open input file list. aborting." << endl;

  //======================================================
}
