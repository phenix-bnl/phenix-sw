// 
// This is a simplified emcreco sample macro, to be used
// with Valgrind.
// Simplified means :
// a) we only register emc as SubsysReco (thus
// we'll miss vertex information, e.g. you will get outputs like : 
// "No information (vtxout) about vertex. Assuming (0,0,0)" you can 
// consider them harmless)
// b) no output manager at all. So no output dst whatsoever.
// (thus saving some memory so valgrind can survive)
//

void valgrind_emcreco(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");
  gSystem->Exec("mkdir ./valgrind_emcreco");

  Fun4AllServer* se = Fun4AllServer::instance(); 
  SubsysReco *head    = new HeadReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *emc = new EmcReco3();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_emcreco");

  se->registerSubsystem(head);
  se->registerSubsystem(bbc);
  se->registerSubsystem(vtx);
  se->registerSubsystem(emc);
  se->registerSubsystem(dmp);
 
  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_emcreco.root");
  se->registerOutputManager(out);
  
  pfileopen(input);
  prun(nevents);
  se->End();
}
