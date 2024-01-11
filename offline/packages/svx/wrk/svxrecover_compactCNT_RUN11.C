
void svxrecover_compactCNT_RUN11(
  int nEvents = 0,
  char* inputfile  = "/phenix/zdata03/phnxreco/VTX/lebedev/data/DST_0000347129-0000_testtest_100evts.root"
//  char* outputfile = "test.root"
)
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libcompactCNT.so");

  Fun4AllServer *se = Fun4AllServer::instance();

  SubsysReco* recoverSvx = new RecoverSvxHits();
  recoverSvx->Verbosity(2);
  se->registerSubsystem(recoverSvx);

  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("Dstin1");
  in1->AddFile(inputfile);
  se->registerInputManager(in1);

//  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("Out", outputfile);
//  se->registerOutputManager(out);

  se->run(nEvents);
  se->End();
}













