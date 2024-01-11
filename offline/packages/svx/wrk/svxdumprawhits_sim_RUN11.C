void svxdumprawhits_sim_RUN11(
  Int_t nEvents = 0,
  char *filein="/phenix/scratch/lebedev/DST_0000347129-0000.root",
  //char *fileout = "/phenix/subsys/vtx/singles/svxhits/svxhits_347129_MB_vtx1.dat",
  char *fileout = "/phenix/scratch/lebedev/svxhits_347129_MB_vtx1.dat"
) {

  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libcompactCNT.so");
  gSystem->Load("libsimreco.so");
//  gSystem->Load("liblvl2.so");
//  gSystem->Load("libEWG.so");
//  gSystem->Load("libFilterReco.so");
//  gSystem->Load("librecal.so");
//  gSystem->Load("libKalFit.so");
//  gSystem->Load("/phenix/u/workarea/lebedev/chi_c_run8dAu/install/lib/libpairobjee.so");
//  gSystem->Load("/phenix/u/workarea/lebedev/chi_c_run8dAu/install/lib/libpairana.so");

  cout << "Analysis started " << endl;
  gSystem->Exec("date");

//  // recoConsts setup
//  recoConsts *rc = recoConsts::instance();
//  rc->set_IntFlag("RUNNUMBER",251500);  // This is the reference run number used in pp 200 GeV production (Run5) 

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  svxpar->set_OffsetVtxToCnt(0.,0.,0.);
  svxpar->set_OffsetEastToWest(0.,0.,0.);
  svxpar->set_BeamCenter(0.,0.);
  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_GeometryFileName("svxPISA.par.my");
  /// setting for masking of hot/dead channels
  /// If you want to set hot/dead channels from files, please remove
  /// the following comment out.
  //    svxpar->set_ReadPixelMapFromFile(1);
  //    svxpar->set_ReadStripHotDeadFromFile(1);
  //    svxpar->set_PixelHotDeadChipFileName("pixelhotdeadchipmap.txt");
  //    svxpar->set_PixelHotDeadPixelFileName("pixelhotdeadpixelmap.txt");
  //    svxpar->set_StripHotDeadHybridsFileName("BadStripHybrids_347129.txt");
  //    svxpar->set_StripHotDeadFileName("striphotdeadmap.txt");
  se->registerSubsystem(svxpar);

  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  svxhotdead->Verbosity(0);
  se->registerSubsystem(svxhotdead);

  SvxMergeRawHits* svxmerge = new SvxMergeRawHits();
  svxmerge->set_mergefilename(fileout);
  svxmerge->set_merge(false);  // dump
  //svxmerge->set_ZVtxCut(-1.0,1.0);
  //svxmerge->set_BbcCut(-9999.,9999.);
  svxmerge->set_BbcCut(910.,9999.); // ~20% most central
  //svxmerge->set_whichVertex("BBC");
  //svxmerge->set_whichVertex("SVX_PRECISE");
  //svxmerge->set_Nskip(5000);
  svxmerge->Verbosity(1);
  se->registerSubsystem(svxmerge);

  Fun4AllInputManager *in1 = new Fun4AllNoSyncDstInputManager("DSTin1","DST");
  in1->Verbosity(0);
  se->registerInputManager(in1);

  cout << "Analysis started " << endl;
  gSystem->Exec("date");

  //in1->AddListFile("list_ccbar_ckin3_3.txt");
  //in1->AddListFile("list_bbbar.txt");
  //in1->AddFile("/phenix/subsys/vtx/singles/simdst/simDST_pipm_005.root");
  in1->AddFile(filein);
  se->run(nEvents);

  se->End();

  cout << "Analysis finished " << endl;
  gSystem->Exec("date");

}

