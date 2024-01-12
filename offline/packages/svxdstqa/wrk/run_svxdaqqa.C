void run_svxdaqqa(int nevt = 0, char *filein="CNT_MB_run11auau_200GeV_pro90-0000349206-9000.root")
{

  cout<<"input : "<<filein<<endl;
 
  size_t find_s;
  string sfilein = filein;
  find_s=sfilein.find("-");
  cout<<"find_s : "<<find_s<<endl;

  int runnum=-1, agsegnum=-1;
  bool fsearch = false;
  if (find_s!=string::npos){
    string s_runseg  = sfilein.substr(find_s+1,16);
    size_t find_bar  = s_runseg.find("-");
    cout<<"pos : "<<find_bar<<endl;
    if (find_bar!=string::npos){
      string s_runnum = s_runseg.substr(0, find_bar);
      string s_segnum = s_runseg.substr(find_bar+1, find_bar);
      runnum   = atoi(s_runnum.c_str());
      agsegnum = atoi(s_segnum.c_str());
      //cout<<"runnumber and segment: "<<s_runnum.c_str()<<" "<<s_segnum.c_str()<<endl;
      cout<<"runnumber and segment: "<<runnum<<" "<<agsegnum<<endl;
      fsearch = true;
    }
  }
  if(!fsearch){
    cout<<"Check input file name"<<endl;
    return;
  }
  
  char outputfile1[200]; 
  sprintf(outputfile1,"SvxDaqQA_%010d-%04d.root", runnum, agsegnum);
  cout << "Outputfile " << outputfile1 << endl;

  char *outputfile=outputfile1;

  //-------------------------------------------------------
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  //gSystem->Load("libreactionplane.so");
  //gSystem->Load("librecal.so");
  //gSystem->Load("libsimreco.so");
  gSystem->Load("libcompactCNT.so");
  //gSystem->Load("libRun11VTXana.so");
  //gSystem->Load("libKalFit.so");
  gSystem->Load("libsvx.so");
  gSystem->Load("libSvxDstQA.so");

  //gSystem->Load("libRawTrackMatching.so");


  Fun4AllServer *se = Fun4AllServer::instance();
  // se->Verbosity(1);


  recoConsts *rc = recoConsts::instance();


  SvxParManager *svxpar = new SvxParManager();
  se->registerSubsystem(svxpar);

  //=======================
  // install my data analysis module
  //=======================
  SvxDaqQA *ana = new SvxDaqQA(outputfile, agsegnum);
  se->registerSubsystem(ana);

  //=======================
  // Input manager(s)
  //=======================
  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  in1->AddFile(filein); 
  se->registerInputManager(in1);

  cout << "Analysis started " << endl;
  gSystem->Exec("date");
 
  cout <<endl<<endl;
  cout <<"DST FILE is opened"<<endl;
  cout <<endl<<endl;

  //==========================
  // now start processing the data
  //==========================
  se->run(nevt);
  se->fileclose("DSTin1"); 
  se->End();

  cout << "Analysis finished " << endl;
  gSystem->Exec("date");
}


