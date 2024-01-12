/*!
  \file phpythia.C
  \brief main macro that generates muons from J/psi decay in muon arm acceptance and embedding.	
*/
void phpythia(
  const int nevents = 100000, 
  const char* vertex_file = "event.txt",
  const char* pythia_out = "pythia_files/pythia_jpsi_fvtx-1.root",
  const char* normalization_out = "normalization/normalization.root",
  int arm = 0,
  double pt_min = 0.0
  )
{
  gSystem->Load("libfun4all.so");  // framework + reco modules
  gSystem->Load("/direct/phenix+u/workarea/slash/pythia/libPythia6PDF.so");  // library to use LHAPDF
  gSystem->Load("libPHPythia.so");
  gSystem->Load("libPHPythiaEventGen.so");
  gSystem->Load("libPHPyParticleSelect.so");
  gSystem->Load("libPHPyTrigger.so");
  gSystem->Load("libsimreco.so");       // framework + reco modules 

  bool write_oscar_file = false;
  
  recoConsts *rc = recoConsts::instance();
  //  rc->set_IntFlag("RUNNUMBER",307084); // run10 AuAu
  //  rc->set_IntFlag("RUNNUMBER",367607); // run12 510pp 
  rc->set_IntFlag("RUNNUMBER",375910); // run12 CuAu

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();
  //  se->Verbosity(0);
  
  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  {
    gSystem->Load( "libmuon_util" );
    MuonCounter* counter = new MuonCounter();
    counter->set_event_dump( 1 );
    se->registerSubsystem( counter );
  }
  
  PHPythia *phpythia = new PHPythia();
  se->registerSubsystem(phpythia);

  // trigger
  PHPyJPsiMuonTrigger* trigger = new PHPyJPsiMuonTrigger(); // for J/psi 
  //  PHPyUpsilonMuonTrigger* trigger = new PHPyUpsilonMuonTrigger(); // for Upsilon
  //  PHPyDYMuonTrigger* trigger = new PHPyDYMuonTrigger();  // for DY
  //  PHPyOniaMuonTrigger* trigger = new PHPyOniaMuonTrigger();
  //  trigger->set_parent_selection(PHPyOniaMuonTrigger::PsiPrime);

  //  arm selection
  //  if( arm == 0 ) trigger->set_arm_selection( PHPyJPsiMuonTrigger::South );
  //  else  trigger->set_arm_selection( PHPyJPsiMuonTrigger::North ); // for J/psi
  //  if( arm == 0 ) trigger->set_arm_selection( PHPyUpsilonMuonTrigger::South );
  //  else  trigger->set_arm_selection( PHPyUpsilonMuonTrigger::North ); for Upsilon
  
  // pt range
  trigger->set_pt_min( pt_min );
  
  trigger->set_normalization_filename( normalization_out );
  trigger->vertex_shift_module()->SetVtxFileVersion( 1 ); //version1 = 0, version2 = 1
  trigger->vertex_shift_module()->SetVtxFile( vertex_file ); 
  se->registerSubsystem( (SubsysReco*) trigger );

  // particle selection
  PHPySelectOnia *particle_selection = new PHPySelectOnia();
  //  PHPySelectDY *particle_selection = new PHPySelectDY();
  se->registerSubsystem( (SubsysReco*) particle_selection );
  
  // useless
  Fun4AllDummyInputManager *in1 = new Fun4AllDummyInputManager("DSTin1", "DST");
  se->registerInputManager(in1);

  // output manager
  Fun4AllDstOutputManager *dst_manager  = new Fun4AllDstOutputManager( "PHPYTHIA", pythia_out );
  dst_manager->AddNode("Sync");
  dst_manager->AddNode("PHPythiaHeader");
  dst_manager->AddNode("PHPythia");
  se->registerOutputManager(dst_manager);

  // oscar output manager
  if( write_oscar_file && oscar_out )
  {
    PHPyOscarOutputManager *oscar_manager  = new PHPyOscarOutputManager( "OSCAR", oscar_out );
    se->registerOutputManager(oscar_manager);
  }
  
  // run
  se->run(nevents);
  se->End();
  
  delete se;
 
  cout << "Completed reconstruction." << endl;
  
}
