void Fun4EMCTree(char *cntfile = "cnt.root", char *pwgfile = "pwg.root",char *clusterfile = "clusters.root", char *hitfile = "nhits.root", char *testfile = "test.root")
	{

  //Introduce ourself to the human
  std::cout << "Fun4EMCTree " << std::endl;
  std::cout << " - cntfile:  " << cntfile      << std::endl;
  std::cout << " - pwgfile:  " << pwgfile      << std::endl;
  std::cout << " - cluterfile: " << clusterfile << std::endl;
  std::cout << " - hitfile:    " << hitfile     << std::endl;
  std::cout << " - testfile:   " << testfile    << std::endl;

   //-----------------Libraries
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("../lib/libEmcOffCal.so");

  //-----------------Fun4All server

  Fun4AllServer *se = Fun4AllServer::instance();

  string FileName = cntfile;
  int length = FileName.size();
  string FileTag = FileName.substr(length-21,17);

  istringstream RUNNUMBER(FileTag.substr(1,10));
  int runnumber;
  RUNNUMBER >> runnumber;

  string WARNMAP = "";

  int n_evt_max = 0; //200000;
  double mom_cut = 0.1;
  double trk_cut = 0.5;
  int bl_apply_warnmap     = false;
  int bl_with_partesum_etc = true; // used for energy calibration

  SubsysReco *sub_clus = new MakeClusterTree(
         runnumber, clusterfile, WARNMAP.c_str(), 
         n_evt_max, mom_cut, trk_cut, bl_apply_warnmap, bl_with_partesum_etc);
  SubsysReco *sub_nhit  = new NhitTree(runnumber, hitfile); 
  //SubsysReco *sub_test  = new TestSubsysReco(runnumber, testfile);

  se->registerSubsystem(sub_clus);
  se->registerSubsystem(sub_nhit);

   Fun4AllInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");

   se->registerInputManager(in1);
      
   in1->AddFile(cntfile);

   Fun4AllInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST");
   se->registerInputManager(in2);

   in2->AddFile(pwgfile);


   std::cout << "####" << std::endl
             << "#### Analysing DST file: " << cntfile << std::endl
             << "####" << std::endl << std::flush;


   se->run(100);
   cout << "    processed events = " << se->TotalEvents() << endl;

   se->End(); //// call End() method

   string histos("histos.root");
   se->dumpHistos(histos.c_str());

}
