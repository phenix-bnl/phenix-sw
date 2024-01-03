void valgrind_dchreco(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_dchreco");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  
  //  Some DCH fina adjustment parameters...
  rc->set_FloatFlag("DVEASTSCALE", 0.985);
  rc->set_FloatFlag("T0EASTSHIFT",-6.65);

  rc->set_FloatFlag("DVWESTSCALE", 0.985);
  rc->set_FloatFlag("T0WESTSHIFT",-2.21);

  rc->set_FloatFlag("EASTMAXSAG", -0.017);
  rc->set_FloatFlag("WESTMAXSAG", -0.017);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();

  SubsysReco *bbc     = new BbcReco();
  SubsysReco *zdc     = new ZdcReco();

  SubsysReco *t0      = new T0Reco();
  SubsysReco *pad     = new PadReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *dch     = new DchReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_dchreco");

 ////////////////////////////////// 
 // Register SubSystems 
 ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(trig);

  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);

  se->registerSubsystem(t0);
  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(dch);
  se->registerSubsystem(dmp);

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","dch.root");
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(input);
  se->registerInputManager(in);
  se->run(nevents);
  se->End();
  delete se;

  cout << "Successfully Completed Analysis." << endl;
  delete se;
  gSystem->Exit(0);
}
