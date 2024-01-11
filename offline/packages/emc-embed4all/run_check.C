void run_check(int nevents=0,
	       const char* output="checkMerged_third.root")
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
  //  se->Verbosity(1);

  const char* creal = "PWG_MinBias_run4AuAu_Central_62_4GeV_v01_pro53-0000122929-0011.root";

  const char* cmerged = "/phenix/data06/aphecetc/run4/embedding/testMerged_third.root";

  Fun4AllInputManager* im0 = new Fun4AllDstInputManager("merged","DST","MERGED");
  Fun4AllInputManager* im1 = new Fun4AllDstInputManager("real","DST");

  se->registerInputManager(im0);
  se->registerInputManager(im1);

  se->registerSubsystem(new EmcEmbedReader("MERGED","TOP"));

  se->fileopen("merged",cmerged);
  se->fileopen("real",creal);

  gBenchmark->Start("check");

  se->run(nevents);

  gBenchmark->Show("check");

  se->EndRun();

  se->dumpHistos(output);
}
