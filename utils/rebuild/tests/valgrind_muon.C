void valgrind_muon(const int nevents=1000, const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libmutoo_subsysreco.so");
  gSystem->Load("libMWGOO.so");
  gSystem->Load("libphnodedump.so");
  gSystem->Exec("mkdir ./valgrind_muon");
 
  Fun4AllServer *se = Fun4AllServer::instance();
  SubsysReco *head = new HeadReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *unpack = new MuonUnpackPRDF();
  SubsysReco *muioo = new MuiooReco();
  SubsysReco *mutoo = new MuonDev();          // mutr reconstruction
  SubsysReco *mwg = new MWGOOReco( new MWGInclusiveNanoCutsv2() );  // muon nanoDST
  MuonReco *muon = new MuonReco();
  muon->set_n_LL1_hit_required(4);	 
  muon->SetDestinationFile("Mu_DST_muon.root");

  Dumper *dmp = new Dumper();
  dmp->SetOutDir("./valgrind_muon");
  se->registerSubsystem(head);
  se->registerSubsystem(bbc);
  se->registerSubsystem(unpack);
  se->registerSubsystem(muioo);
  se->registerSubsystem( mutoo );
  se->registerSubsystem( mwg );
  se->registerSubsystem( muon );
  se->registerSubsystem(dmp);
  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("DSTOUT","valgrind_muon.root");
  se->registerOutputManager(out);
  pidentify(0);
  pfileopen(input);
  prun(nevents);
  se->End();
}

