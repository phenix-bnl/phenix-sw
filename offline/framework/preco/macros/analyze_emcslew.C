void analyze_emcslew(int nEvents=0, char *inFileList="filelist_short.txt", char *outFile="out_emcslew2_short.root")
{
  //  gSystem->Load("libdstqa.so");
  cout << "loading emcslewinganalyzer lib" << endl; gSystem->Load("libemcslewingAnalyzer.so");
  cout << "loading fun4allfuncs lib" << endl;       gSystem->Load("libfun4allfuncs");
  //  gSystem->Load("libemcOMpg.so");
  cout << "loading onlcalserverfuncs lib" << endl;  gSystem->Load("libonlcalserver_funcs.so");
  cout << "loading onlreco lib" << endl;            gSystem->Load("libonlreco.so");
  cout << "loading calemctof lib" << endl;          gSystem->Load("libcalemctof.so");
  cout << "loading PgCalInstance lib" << endl;      gSystem->Load("libPgCalInstance.so");
  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  /////////////////////////////////////////////////////////////////
  //  Analysis Modules ...
  SubsysReco *emcslewing = new EmcslewingAnalyzer(outFile);
  EmctofrecalReco *recal = new EmctofrecalReco (3, "PHCentralTrack");
  //EmctofrecalReco *recal = new EmctofrecalReco (0, "PHCentralTrack");
                              //for SlewScheme: 0--> no slewing, 1-->Tom's, 2-->all towers, 3-->towerbytower
  

  //reset flags, true is default
  recal->Verbosity   ( true);
  recal->Do62GeV     (false);
  //recal->DoDeltaT    (false); 
  recal->DoDeltaT    ( true); 
  recal->DoLeastCount(false);
  recal->DoLaserLC   (false);
  recal->DoRunbyRun  ( true);
  //recal->DoRunbyRun  (false);
  //  recal->Print();

  se->registerSubsystem(recal);
  se->registerSubsystem(emcslewing);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in);
  in->Verbosity(0);
  
  ///////////////
  //while thru file list and run
  char dstfile[500];
  ifstream runlist(inFileList);
  while (runlist.getline(dstfile, 500)) // getline prevents overflow beyond 500 chars
    {
      in->AddFile(dstfile); // load the filelist into the Input Manager
    }
  se->run(0);
  se->End();
  //recal->Print();
  // se->dumpHistos(outFile);

  ////////////////////////////////////////////////////////////////
  //  OK, now loop over all the input files...
//  char dstfile[500];
//  ifstream runlist(inFileList);
//  int i=0;
//
//  runlist.getline(dstfile, 500);
//  cout << "Opening " << i++ << ": " << dstfile << endl;
//  se->fileopen("DSTin1", dstfile);
//  se->run(0);
//
//  while (runlist.getline(dstfile, 500)) // getline prevents overflow beyond 500 chars
//    {
//      cout << "Opening " << i++ << ": " << dstfile << endl;
//      se->fileopen("DSTin1", dstfile);
//      se->run(0);
//    }
//  
//  se->End();
//  //se->dumpHistos(outFile);

}
