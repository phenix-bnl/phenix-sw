void analyze_rndm(char *inFileList = "filelist_short.txt", char *outFile = "test.root")
{
  cout << "loading fun4all lib" << endl;       gSystem->Load("libfun4all.so");
  cout << "loading fun4allfuncs lib" << endl;       gSystem->Load("libfun4allfuncs.so");
  cout << "loading lvl2 lib" << endl;            gSystem->Load("liblvl2.so");
  //  cout << "loading lib RandomTrack" << endl;    gSystem->Load("libRandomTrack.so");
  cout << "loading lib CabanaBoy" << endl;    gSystem->Load("libCabanaBoy.so");

  /////////////////////////////////
  //  Make and configure a MasterRecalibrator
  MasterRecalibrator *mr = MasterRecalibrator::instance();
  cout << "master recalibrator made" << endl;

  /////////////////////////////////
  //  Make and configure a RandomTracks
  //  RandomTracks *rt = new RandomTracks("PHCentralTrack");
  SubsysReco *rt = new RandomTracks("PHCentralTrack");
  //  rt->Verbosity(1);
  cout << "random tracks made" << endl;

  /////////////////////////////////
  //  Make and configure a CabanaBoy
   cbMasterHistos *mh = new DCHHistos;
   cbMasterCutter *lc = new DCHCutter;

   CabanaBoy *cb2 = new CabanaBoy(10,5,1,"PHCentralTrack"); //10,3,1
   cb2->SetHistoFileName(outFile);
   cb2->setCuts(lc);
   cb2->setHistos(mh);
   cb2->setPoolType(CabanaBoy::AkibaPools);
   cb2->setFastMom (true);
   cb2->setReactionPlaneSelectionType(CabanaBoy::ReactionPlaneNotUsed); //means rp not used
//   cb2->Verbosity(1);

  /////////////////////////////////
  //  Register with Fun4All
  Fun4AllServer *se = Fun4AllServer::instance();
  se->registerSubsystem(mr);
  se->registerSubsystem(rt);
  se->registerSubsystem(cb2);


  /////////////////////////////////
  //  Input Manager
  Fun4AllDstInputManager *in = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in);
  in->Verbosity(1);
  in->AddListFile(inFileList); // load the filelist into in-Manager
  in->Print(); // print out info from input mgr

  ////////////////////////////////
  //  Run it!!!!
  se->run(0); //I think 0 means run all files, not just first n before was 5001
  se->End();
  se->dumpHistos(outFile);
  cout << "done." << endl;

}
