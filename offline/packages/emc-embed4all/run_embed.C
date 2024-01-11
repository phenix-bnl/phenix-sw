void run_embed(int nevents=0)
{
  gSystem->Load("libemcEmbed4all.so");
  
  bool debug=false;
  bool check=false;

  if ( debug ) 
    {
      cout << "PID = " << gSystem->GetPid() << endl;
      int test;
      cin >> test;
    }

  Fun4AllServer* se = Fun4AllServer::instance();
  //  se->Verbosity(1);

  const char* creal_clu = "";

  const char* csimu = "/phenix/data06/ptarjan/run04/sim/pass2/sim_dst-0000053071-0050.root";
  const char* creal_twr = "/phenix/data06/saskia/embedding/DST_EMCTwr_MinBias_run4AuAu_Central_62_4GeV_v01_pro53-00000122783-0011.root";
  const char* creal_eve = "PWG_MinBias_run4AuAu_Central_62_4GeV_v01_pro53-0000122783-0011.root";

  const char* output = "output.root";

  const char* imreal = "real";
  const char* imsimu = "simu";
  const char* imeve = "eve";

  Fun4AllInputManager* im1 = new Fun4AllDstInputManager(imreal,"DST","REAL");
  Fun4AllInputManager* im2 = new Fun4AllDstInputManager(imeve,"DST","REAL");

  // This im3 is only needed when using EmcEmbedChecker. Not for production.
  Fun4AllInputManager* im3 = 0;

  if ( check ) 
    {
      im3 = new Fun4AllNoSyncDstInputManager("realcheck","DST","REAL");
    }

  se->registerInputManager(im2);
  se->registerInputManager(im1);

  if ( im3 ) 
    {
      se->registerInputManager(im3);
    }

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
  // zsel->Verbosity(1);

  //  SubsysReco* zsel = new EmcEmbedVertexSelector(-7,7,"REAL","SIMU");

  se->registerSubsystem(zsel);
  se->registerSubsystem(new EmcEmbedDriver("REAL","SIMU","TOP",imreal));
  //  se->registerSubsystem(new EmcEmbedSimDeadTower("REAL","SIMU"));
  se->registerSubsystem(new EmcSimTowerSmearer("SIMU"));
  se->registerSubsystem(new EmcEmbedTowerMerger("REAL","SIMU","TOP"));
  se->registerSubsystem(new EmcEmbedReclusterizer("REAL","SIMU","TOP"));
  se->registerSubsystem(new EmcEmbedMergedClusterUpdater("TOP"));
  se->registerSubsystem(new EmcEmbedOutputMover("REAL","SIMU","TOP"));
  if (check)
    {
      se->registerSubsystem(new EmcEmbedChecker("REAL","SIMU","TOP"));
    }

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
  se->fileopen(imeve,creal_eve);
  if ( im3 ) 
    {
      se->fileopen("realcheck",creal_clu);
    }

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
