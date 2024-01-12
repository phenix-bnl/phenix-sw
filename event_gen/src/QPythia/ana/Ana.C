void Ana(
  const int nevents = 10, 
  const char *inputfile = "input/qpyjets_ckin8_qhat0_1000evts.root",
  const char *outputfile = "output/qpyjettree_ckin8_qhat0_1000evts.root"
  )
{

  gSystem->Load("libfun4all.so");	        // framework + reco modules
  gSystem->Load("libQPythiaAna.so");
  gSystem->Load("libPHPythiaEventGen.so");
  gSystem->Load("libPHPythia.so");

  Fun4AllServer *se = Fun4AllServer::instance();

  QPythiaAna *ana = new QPythiaAna("AntiKt04JetsStable",outputfile);
  se->registerSubsystem(ana);

  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  in1->AddFile(inputfile);
  se->registerInputManager(in1);

  // run over all events
  se->run(nevents);  
  se->End();

}

