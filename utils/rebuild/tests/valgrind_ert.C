void valgrind_ert(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_ert");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////


  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *head    = new HeadReco();
  SubsysReco *ert     = new ErtReco();

  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_ert");

 ////////////////////////////////// 
 // Register SubSystems 
 ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(ert);

  se->registerSubsystem(dmp);

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_ert.root");
  se->registerOutputManager(out);
  pfileopen(input);
  pidentify(0);
  prun(nevents);
  se->End();


  cout << "Successfully Completed Analysis." << endl;
}
