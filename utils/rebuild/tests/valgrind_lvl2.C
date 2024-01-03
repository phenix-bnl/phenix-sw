void valgrind_lvl2(const int nevts = 1000, const char *prdffile = "/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000117455-0000.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("liblvl2.so");
  gSystem->Load("libphnodedump.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  gSystem->Exec("ln -s /afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN5_REAL/Run5CuCu_62gev_TriggerSetupFile .");
  gSystem->Exec("mkdir ./valgrind_lvl2");
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("LVL2_REAL_DATA",1); // not a pisa file 
  rc->set_IntFlag("LVL2_YEAR",5);   //.. run 4 database files and triggers
  rc->set_IntFlag("LVL2_USE_ASCII_DB",1);  // Read from directory
  rc->set_CharFlag("LVL2_DB_DIR","/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/cucu_production/RUN5_REAL");
  rc->set_CharFlag("TRIGSETUPFILE","Run5CuCu_62gev_TriggerSetupFile");

  rc->set_CharFlag("Run2Lvl2AlgoName", "");   // means take ALL data events
  rc->set_IntFlag("FORCE_LVL2",1); // force Lvl2 to run 




  SubsysReco *lvl2statseval  = new Lvl2StatsEval();
  lvl2statseval->Verbosity(0);

  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *lvl2reco = new Lvl2Reco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_lvl2");

  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(lvl2reco);
  se->registerSubsystem(lvl2statseval);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_lvl2.root");
  se->registerOutputManager(out);

  pidentify(0);
  pfileopen(prdffile);

  prun(nevts);
  se->End();

}














