// trigger selectors, the string in the ctor is the string
// the Output Manager needs as argument to the AddEventSelector
// method

void trigselmuon()
{
  Fun4AllServer *se = Fun4AllServer::instance();
  TrigSelect *tsel = new TrigSelect("MUON");
  tsel->AddTrigger("MUIDS_1D&BBCLL1");
  tsel->AddTrigger("MUIDN_2D&BBCLL1");
  //tsel->Verbosity(2);
  se->registerSubsystem(tsel);
}

void trigselgamma()
{
  Fun4AllServer *se = Fun4AllServer::instance();
  TrigSelect *tsel = new TrigSelect("ERTGAMMA");
  tsel->AddTrigger("ERT_Gamma1&BBCLL1");
  tsel->AddTrigger("ERT_Gamma2&BBCLL1");
  tsel->AddTrigger("ERT_Gamma3&BBCLL1");
  //tsel->Verbosity(2);
  se->registerSubsystem(tsel);
}

void trigselelectron()
{
  Fun4AllServer *se = Fun4AllServer::instance();
  TrigSelect *tsel = new TrigSelect("ERTELECTRON");
  tsel->AddTrigger("ERT_Electron&BBCLL1");
  //tsel->Verbosity(2);
  se->registerSubsystem(tsel);
}

void trigselminbias()
{
  Fun4AllServer *se = Fun4AllServer::instance();
  TrigSelect *tsel = new TrigSelect("MINBIAS");
  tsel->AddTrigger("MINBIAS");
  //tsel->Verbosity(2);
  se->registerSubsystem(tsel);
}
