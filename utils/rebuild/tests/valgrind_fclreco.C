// 
// This is a simplified fclreco sample macro, to be used
// with Valgrind.
// Simplified means :
// a) we only register fcl as SubsysReco (thus
// we'll miss vertex information, e.g. you will get outputs like : 
// "No information (vtxout) about vertex. Assuming (0,0,0)" you can 
// consider them harmless)
// b) no output manager at all. So no output dst whatsoever.
// (thus saving some memory so valgrind can survive)
//

void valgrind_fclreco(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libphnodedump.so");

  gSystem->Exec("mkdir ./valgrind_fclreco");

  Fun4AllServer* se = Fun4AllServer::instance(); 
  SubsysReco *fcal = new FcalReco();
  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_fclreco");

  se->registerSubsystem(fcal);
  se->registerSubsystem(dmp);

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_fclreco.root");
  se->registerOutputManager(out);
  pfileopen(input);
  prun(nevents);
  se->End();
}
