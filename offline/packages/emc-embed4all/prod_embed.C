void prod_embed(int nevents=0,
		const char* csimu,
		const char* creal_twr,
		const char* creal_pwg,
		const char* output)
{
  gSystem->Load("libemcEmbed4all.so");
  
  bool debug=false;
  if ( debug ) 
    {
      cout << "PID = " << gSystem->GetPid() << endl;
      int test;
      cin >> test;
    }

  Fun4AllServer* se = Fun4AllServer::instance();

  const char* imreal = "real";
  const char* impwg = "pwg";

  Fun4AllInputManager* im1 = new Fun4AllDstInputManager(imreal,"DST","REAL");
  Fun4AllInputManager* im2 = new Fun4AllDstInputManager(impwg,"DST","REAL");

  se->registerInputManager(im2);
  se->registerInputManager(im1);

  const float zrealinterval = 30.0; //+-30 cm
  const float deltaz = 5.0; // cm between real and simu vertices.
  const size_t maxnumberofiterations = 50;
  const size_t poolfractiontouse = 1.0;

  SubsysReco* zsel = new EmcEmbedEventSelector(csimu,
					       zrealinterval,
					       deltaz,
 					       "REAL","SIMU",
 					       maxnumberofiterations,
 					       poolfractiontouse);
  
  se->registerSubsystem(zsel);
  se->registerSubsystem(new EmcEmbedDriver("REAL","SIMU","TOP",imreal));
  se->registerSubsystem(new EmcEmbedSimDeadTower("REAL","SIMU"));
  se->registerSubsystem(new EmcSimTowerSmearer("SIMU"));
  se->registerSubsystem(new EmcEmbedTowerMerger("REAL","SIMU","TOP"));
  se->registerSubsystem(new EmcEmbedReclusterizer("REAL","SIMU","TOP"));
  se->registerSubsystem(new EmcEmbedMergedClusterUpdater("TOP"));
  se->registerSubsystem(new EmcEmbedOutputMover("REAL","SIMU","TOP"));

  // Output manager
  Fun4AllOutputManager* dst = new Fun4AllDstOutputManager("DST",output);

  dst->AddNode("fkin");
  dst->AddNode("primary");
  dst->AddNode("dEmcGeaTrack");
  dst->AddNode("dEmcGeaClusterTrack");
  dst->AddNode("dEmcGeaTrackCluster");
  dst->AddNode("emcSimClusterContainer");
  dst->AddNode("emcClusterContainer");
  dst->AddNode("Sync");
  dst->AddNode("PHGlobal");
  dst->AddNode("VtxOut");
  dst->AddEventSelector("EmcEmbedOutputMover");

  se->registerOutputManager(dst);

  se->fileopen(imreal,creal_twr);
  se->fileopen(impwg,creal_pwg);

  gBenchmark->Start("embed");

  TDatime d1;

  se->run(nevents);

  TDatime d2;

  gBenchmark->Show("embed");

  se->EndRun();

  delete zsel;

  d1.Print();
  d2.Print();
}
