void valgrind_crkreco(const int nevents=1000,
		      const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000117455-0000.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  cout << "Creating softlink for crk_cabling.txt" << endl;
  gSystem->Exec("ln -s /afs/rhic/phenix/software/calibration/run4/crk_cabling.txt .");
  Fun4AllServer* se = Fun4AllServer::instance(); 

  SubsysReco *crk = new CrkReco();

  se->registerSubsystem(crk);

  pfileopen(input);
  prun(nevents);
  se->End();
  cout << "Removing softlink for crk_cabling.txt" << endl;
  //gSystem->Exec("rm -f crk_cabling.txt");
}
