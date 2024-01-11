//void ReadAndRecal_compactCNT(const char *indst)
void ReadAndRecal_compactCNTwithSVX()
{
  gSystem->Load("librecal.so");
  gSystem->Load("libcompactCNT.so");
//  gSystem->Load("libsimreco.so");
  gSystem->Load("libKalFit.so");
//  gSystem->Load("libsvxcgl.so");
  Fun4AllServer *se = Fun4AllServer::instance();

  char outfilename[500];
  sprintf(outfilename, "/phenix/scratch/lebedev/allout_new.root");

  //==============================================
  // These modules read back the compactCNT files
  // and create hits objects for each subsystem
  //================================================

  se->registerSubsystem(new RecoverTrackProjections());
  se->registerSubsystem(new RecoverTrackLineProjections());
  se->registerSubsystem(new RecoverTrackPathLengths());
  se->registerSubsystem(new RecoverTrackHits());
  se->registerSubsystem(new RecoverDchHits());
  se->registerSubsystem(new RecoverPadHits());
//  se->registerSubsystem(new RecoverTofeHits());
//  se->registerSubsystem(new RecoverTofwHits());
  se->registerSubsystem(new RecoverCrkHits());
//  se->registerSubsystem(new RecoverTecHits());
//  se->registerSubsystem(new RecoverAccHits());
  SubsysReco* recoverSvx = new RecoverSvxHits();
  recoverSvx->Verbosity(1);
  se->registerSubsystem(recoverSvx);
  se->registerSubsystem(new RecoverEmcHits());

  //========================
  // Creates PHCentralTrack
  //========================

  se->registerSubsystem(new CreateCNT());

  //=================================================
  // These modules re-associate hits with tracks and 
  // fill the PHCentralTrack fields
  //==================================================

  se->registerSubsystem(new FillCNT_TrackProjections());
  se->registerSubsystem(new FillCNT_TrackPathLengths());
  se->registerSubsystem(new FillCNT_TrackHits());
  se->registerSubsystem(new FillCNT_DchHits());
//  se->registerSubsystem(new FillCNT_TofeHits());
//  se->registerSubsystem(new FillCNT_TofwHits());
  se->registerSubsystem(new FillCNT_PadHits());
  se->registerSubsystem(new FillCNT_CrkHits());
//  se->registerSubsystem(new FillCNT_TecHits());
//  se->registerSubsystem(new FillCNT_AccHits());
  SubsysReco* fillSvx = new FillCNT_SvxHits();
  fillSvx->Verbosity(1);
  se->registerSubsystem(fillSvx);
  se->registerSubsystem(new FillCNT_EmcHits());

  //===========================================================
  // This is not needed for analysis.
  // It makes some histograms of track associated 
  // fields for the RICH, Aerogel and TEC.
  // The histos are displayed by the macro
  // "offline/analysis/test_compactCNT/display_testnewout.C"
  // They provide quick feedback on whether things worked
  //===========================================================

//  std::string newoutpath( "/phenix/scratch/lebedev/");
//  TestNewOut *testnewout = new TestNewOut();
//  testnewout->SetHistPath(newoutpath);
//  se->registerSubsystem(testnewout);

  // ================================================================
  // Recalibrators
  //=================================================================  

//  MasterRecalibratorManager *mgr = new MasterRecalibratorManager();
//  mgr->Verbosity(5);
//  se->registerSubsystem(mgr);
  
//  Fun4AllInputManager *in = new Fun4AllDstInputManager("Dstin");
//  in->AddFile(indst);
//  se->registerInputManager(in);

  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("Dstin1");
  //in1->AddFile("/phenix/scratch/frawley/run_0000239000_0000240000/PWG_NEW/PWG_New_MinBias_run7AuAu_Central_200GeV_pro78-0000239461-0008.root");
  //in1->AddFile("/phenix/subsys/vtx/lebedev/phpythia/simDST_hijing_20evts.root");
  in1->AddFile("/phenix/subsys/vtx/lebedev/newsim/simdst/simDST_php_20evts_test.root");
  se->registerInputManager(in1);
  

  Fun4AllOutputManager *out = new Fun4AllDstOutputManager("Out", outfilename);
  se->registerOutputManager(out);

  se->run(10);
  se->End();
}
