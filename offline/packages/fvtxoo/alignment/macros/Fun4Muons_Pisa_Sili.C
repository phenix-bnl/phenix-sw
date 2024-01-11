// $Id: Fun4Muons_Pisa_Sili.C,v 1.1 2013/10/16 22:34:21 jinhuang Exp $
/*!
 prdf analysis loop for real data. Unpack the data,
 creates a unreconstructed DST
 */

void
Fun4Muons_Pisa_Sili(int nEvents = 1000, //
    bool use_svx_cluster = true, //
    const char *inputfile = "pisa.root", //
    const char *dstfile = NULL, //
//    const char *dstfile = NULL, //
//    const char *ana_file = "muon_ana_ntuples.root", //
    const char *ana_file = NULL, //
    const char *singlepdstout = "singlemuon_pdst.root", //
    const char *hit_dstout = "hit_dst.root", //
//    const char *dimuonpdstout = "dimuon_pdst.root", //
    const char *dimuonpdstout = NULL, //
    int run_number = 367466)
{

  // flags
  bool use_lvl2 = false;
  bool use_fvtx = true;

  bool write_ndst = false;
  bool write_pdst = true;
//  bool write_pdst = false;
  bool do_eval = false;
  bool write_align_dst = false;
  bool write_dst_reader = true;
//  bool write_dst_reader = false;

  bool fill_mutr_mc_info = true;

  // load libraries
  gSystem->Load("libPHGeant");
  gSystem->Load("libfun4all");
  gSystem->Load("libsimreco_base");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libmuon_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("libfvtx_subsysreco");
  gSystem->Load("liblvl2");
  gSystem->Load("libPythia6.so");
  gSystem->Load("libPHPythia.so");
  gSystem->Load("libPHPythiaEventGen.so");

  cerr << "libraries loaded.\n";

  //TMutDatabaseCntrl::set_database_access( "use_local_global_align_file", true );
  //TMutDatabaseCntrl::set_filename( "use_local_global_align_file", "mut.globalAligConsts.dat" );

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", 1);

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS", 1);
  rc->set_IntFlag("RUNNUMBER", run_number);
  if (use_lvl2)
    {
      // Set Lvl2 flags
      rc->set_IntFlag("LVL2_REAL_DATA", 1);
      rc->set_IntFlag("LVL2_YEAR", 4);
      rc->set_IntFlag("FORCE_LVL2", 1);
      rc->set_IntFlag("LVL2_USE_ASCII_DB", 1);
      rc->set_CharFlag("LVL2_DB_DIR",
          "/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN4_REAL");
      rc->set_CharFlag("Run2Lvl2AlgoName", "");
    }

  //See what happens if we change the b-field scale:
  //mMfmMT::setMapFileScale( 1.00 );

  // mutoo vertex source configuration
  // this allows to print which vertex is used and its value
  //TMutExtVtx::get().set_vtx_source( TMutExtVtx::MC );
  //TMutExtVtx::get().set_smear_z( false );
  TMutExtVtx::get().set_verbosity(MUTOO::NONE);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  // run header and trigger setting
  se->registerSubsystem(new HeadSimreco());
  //    se->registerSubsystem( new TrigSimreco() );

  // vertex simulation
  // puts the vertex from the pisa header node into vtxOut object
  if (true)
    {
      gSystem->Load("libsimreco_base.so");
      VtxSimreco* vtx_reco = new VtxSimreco();
      vtx_reco->SmearZ(true);
      vtx_reco->UseXY(false);
      vtx_reco->OverwriteBBC(true);
      vtx_reco->ZVertexSigma(0.0001);
//      vtx_reco->ZVertexSigma(1.00);
      se->registerSubsystem(vtx_reco);
    }

  // Counter
  se->registerSubsystem(new MuonCounter());

  // global detectors subsystem
  //se->registerSubsystem( new HeadReco() );
  //se->registerSubsystem( new TrigReco( ));
  //se->registerSubsystem( new BbcReco() );
  //se->registerSubsystem( new ZdcReco() );
  se->registerSubsystem(new VtxReco());

  // level2 selection subsystems
  if (use_lvl2)
    {
      gSystem->Load("liblvl2");
      SubsysReco *lvl2reco = new Lvl2Reco();
      lvl2reco->Verbosity(0);
      se->registerSubsystem(lvl2reco);

      SubsysReco *lvl2stats = new Lvl2StatsEval();
      lvl2stats->Verbosity(0);
      se->registerSubsystem(lvl2stats);

      Lvl2RunTrigSelect *lvl2runtrigselect = new Lvl2RunTrigSelect();
      lvl2runtrigselect->AddTrigger("L2MutrDimuonSouthTrigger");
      lvl2runtrigselect->AddTrigger("L2MutrDimuonNorthTrigger");
      lvl2runtrigselect->AddTrigger("L2MuidDimuonSouthTrigger");
      lvl2runtrigselect->AddTrigger("L2MuidDimuonNorthTrigger");

      // tell level2 to reject events which do not trigger on any of the previous
      lvl2runtrigselect->SetReturnCode("ABORT");
      lvl2runtrigselect->Verbosity(0);
      se->registerSubsystem(lvl2runtrigselect);
    }

//   // local level1 subsystem
//   TrigSelect *minBias = new TrigSelect("MB");
//   minBias->AddTrigger("BBCLL1");
//   se->registerSubsystem(minBias);

  // muon prdf unpacker
  MuonUnpackPisa* muon_unpack_pisa(new MuonUnpackPisa());
  muon_unpack_pisa->Verbosity(1);
  muon_unpack_pisa->set_flag(MuonUnpackPRDF::SKIP_ZERO_SUPPRESSION, 1);
  se->registerSubsystem(muon_unpack_pisa);

  // mutoo reconstruction
  se->registerSubsystem(new MuiooReco());
  MuonDev *muon_dev = new MuonDev();
  muon_dev->set_flags(MuonDev::FORCE_LOOKUP);
  se->registerSubsystem(muon_dev);

  // SVX reconstruction"

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  svxpar->set_BeamCenter(0., 0.);
  svxpar->set_OffsetVtxToCnt(0.0, 0.0, 0.0);
  svxpar->set_OffsetEastToWest(0.0, 0.0, 0.0);
  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_GeometryFileName(
      "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/simulation/svxPISA.par.ideal");
  se->registerSubsystem(svxpar);

  SvxSimulator *svxsim = new SvxSimulator();
  svxsim->Verbosity(0);
  se->registerSubsystem(svxsim);

  /*
   SvxDecode *svxdecode = new SvxDecode();
   svxdecode->Verbosity(0);
   svxdecode->includePixel(true);
   svxdecode->includeStripixel(true);
   svxdecode->setAdcOffset(24);
   svxdecode->setAdcCutoff(-24);
   se->registerSubsystem(svxdecode);
   */

  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  svxhotdead->Verbosity(0);
  se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  // svxrec->Load_ThresholdFile("threshold.h");
  svxrec->set_ThisIsSimulation();
  svxrec->set_UseStripThresholdDatbase(true);
  //svxrec->Load_ThresholdFile("threshold_ideal.h");
  svxrec->set_StripixelAdcSumThreshold(0);
  se->registerSubsystem(svxrec);

  SvxPriVertexSeedFinder *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  svxvtxseedfinder->Verbosity(0);
  se->registerSubsystem(svxvtxseedfinder);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);
  se->registerSubsystem(svxstandalone);

  SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

  // fvtx prdf unpacker
  if (use_fvtx)
    {
      TFvtxDatabaseCntrl::set_flag("deadmap_auto_load",false);
      TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database",false);

      FvtxUnpackPisa *fvtx_unpack = new FvtxUnpackPisa();
      fvtx_unpack->set_do_response(true);
      fvtx_unpack->Verbosity(0);
      se->registerSubsystem(fvtx_unpack);

      FvtxReco* fvtxreco = new FvtxReco();
      fvtxreco->run_after_burner(false); // Adds VTX hits to tracking
      fvtxreco->set_use_svx_cluster(use_svx_cluster); // Uses smeared PISA hits if false
      fvtxreco->set_do_mutr_matching(true); // Match FVTX-MuTr tracks and refit or not
//      fvtxreco->set_auto_load_dead_map(false);
      se->registerSubsystem(fvtxreco);

      // Perform FVTX-Mutr track matching and refit track:
      //se->registerSubsystem( new FvtxRecoWithMut() );

      if (do_eval)
        {
          //FvtxMCEval* fvtxeval = new FvtxMCEval();
          FvtxEval* fvtxeval = new FvtxEval("FvtxEval", "fvtx_eval_pisa.root");
          se->registerSubsystem(fvtxeval);
          FvtxMCEval* fvtxeval_mc = new FvtxMCEval("FvtxMCEval",
              "fvtx_mc_eval_pisa.root");
          se->registerSubsystem(fvtxeval_mc);
        }

      if (write_align_dst)
        {

          //  //Perform alignment:
          FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();

          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, true);
          fvtx_global_align->set_flag(FvtxGlobalAlign::DO_EVALUATION, true);
          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);
          fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGNMENT, false);

//        fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT,
              false);

          fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
//          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT,
//              true);

          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, true);
          fvtx_global_align->set_mutr_hit_sigma_min(1);
          fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK,
              false);

          fvtx_global_align->set_vertex_lateral_constraint(0.0001);

          se->registerSubsystem(fvtx_global_align);
        }

    }

  if (do_eval)
    {
      se->registerSubsystem(new MuonAnaTuples());
      se->registerSubsystem(new MuonEval());
    }

  if(fill_mutr_mc_info && ! do_eval)
    {
      // muon evaluation module, used to get MC information in pico DST
      MuonEval* mueval = new MuonEval();
      mueval->set_flags(0); // no ntuple output needed
      se->registerSubsystem (mueval);
    }


  // picoDST
  if (singlepdstout && write_pdst)
    {
      //      se->registerSubsystem( new MpcReco() );
      //      gSystem->Load("librxnp_subsysreco.so");
      //      se->registerSubsystem( new RxnpReco() );
      //      se->registerSubsystem( new RpSumXYReco() ); // recalibrator and rp doesn't work together!

      // MWG
      gSystem->Load("libMWGOO");
      gSystem->Load("libmutrg");
      gSystem->Load("libnanoDST"); // 03Feb2013 beaumier Needed for pDST(?)
      gSystem->Load( "librpc_subsysreco" );
      gSystem->Load( "librpc_muotrackreco" );
      gSystem->Load( "libMWGpico" ); // 03Feb2013 beaumier Needed for pDST (doens't work)

      PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
      MWGcuts->set_dodimu(false);
      MWGFvtxReco * mwgreco = new MWGFvtxReco(MWGcuts);
//        MWGOOReco * mwgreco = new MWGOOReco(MWGcuts);
      se->registerSubsystem(mwgreco);


      // global Reco
      SubsysReco *global = new GlobalReco();
      SubsysReco *global_muons = new GlobalReco_muons();
//        SubsysReco *rpcmuoreco = new RpcMuoReco();

      se->registerSubsystem(global);
      se->registerSubsystem(global_muons);
//        se->registerSubsystem(rpcmuoreco);


      FvtxConeTracklets * tl = new FvtxConeTracklets("FvtxConeTracklets_eval.root");
      if (nEvents>0 && nEvents<100) tl->Verbosity(2); // verbosity
      tl->set_is_sim(true); // which vertex to use, BBC (false) or SIM (true)
//      tl->set_make_eval(true); // output evaluation file
//      tl->set_max_cone(.48);   // change the max cone size
      tl->set_save_tracklet(true); // DEBUG ONLY: over write compact FVTX track object with newly found tracklets.
      se->registerSubsystem(tl);


      gSystem->Load("libpicodst_object.so");
      mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
      msngl->set_bbcz_cut(150.0);
      msngl->set_is_sim(true);
      se->registerSubsystem(msngl);

      if(fill_mutr_mc_info)
        {
          //********************* Mutr MC information Module ******************//
          mFillMCSingleMuonContainer* msngl_mc = new mFillMCSingleMuonContainer();
          se->registerSubsystem(msngl_mc);
        }

      mFillDiMuonContainer* mdi = new mFillDiMuonContainer(false); // do not make mixed events
      se->registerSubsystem(mdi);
      mdi->set_mass_cut(0.5);
      mdi->set_is_sim(true);

      if (write_dst_reader)
        {
          mDSTReader * dr = new mDSTReader();
//          dr->Verbosity(2);
          dr->add_node(mDSTReader::FvtxHit);
          dr->add_node(mDSTReader::FvtxTrk);
          dr->add_node(mDSTReader::FvtxCoord);
//          dr->add_node(mDSTReader::FvtxSvxCluster);
          dr->add_node(mDSTReader::FvtxCompactTrk);
          se->registerSubsystem(dr);
        }

      if (singlepdstout)
        {
          Fun4AllOutputManager *outsmu = new Fun4AllDstOutputManager("Outsmu",
              singlepdstout);
          outsmu->AddNode("Sync");

          outsmu->AddNode("SingleMuonContainer");
          if(fill_mutr_mc_info)  outsmu->AddNode("MCSingleMuonContainer");   // Mutr MC information container
          outsmu->AddNode("DiMuonContainer");

          outsmu->AddNode("VtxOut");
          outsmu->AddNode("PHGlobal");
          outsmu->AddNode("PHGlobal_MUON");

          outsmu->AddNode("PHPythiaHeader");
          outsmu->AddNode("PHPythia");

          outsmu->AddEventSelector("mFillSingleMuonContainer");
//          outsmu->AddEventSelector("mFillDiMuonContainer");

          if (write_dst_reader)
            outsmu->AddNode("DSTReader");
          outsmu->AddNode("SvxClusterList");

          outsmu->AddNode("PHMuoTracksOO");

          se->registerOutputManager(outsmu);
        }

      if (hit_dstout)
        {
          Fun4AllOutputManager *outsmu = new Fun4AllDstOutputManager("OutHit",
              hit_dstout);
          outsmu->AddNode("Sync");

          outsmu->AddNode("SingleMuonContainer");
          if(fill_mutr_mc_info)  outsmu->AddNode("MCSingleMuonContainer");   // Mutr MC information container
          outsmu->AddNode("DiMuonContainer");

          outsmu->AddNode("VtxOut");
          outsmu->AddNode("PHGlobal");
          outsmu->AddNode("PHGlobal_MUON");

          outsmu->AddNode("PHPythiaHeader");
          outsmu->AddNode("PHPythia");

          if (write_dst_reader)
            outsmu->AddNode("DSTReader");
          outsmu->AddNode("SvxClusterList");

          outsmu->AddNode("PHMuoTracksOO");

          se->registerOutputManager(outsmu);
        }

    }

  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////

  // dst
  if (dstfile)
    {
      Fun4AllDstOutputManager *dstManager = new Fun4AllDstOutputManager(
          "DSTOUT", dstfile);

      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");
      dstManager->AddNode("VtxOut");
      dstManager->AddNode("BbcOut");
      dstManager->AddNode("BbcRaw");
      dstManager->AddNode("ZdcOut");
      dstManager->AddNode("ZdcRaw");

      dstManager->AddNode("TMCPrimary");
      dstManager->AddNode("PHPythiaHeader");
      dstManager->AddNode("PHPythia");

      dstManager->AddNode("TrigLvl1");
      dstManager->AddNode("L2Decision");
      dstManager->AddNode("Lvl2OutArray");

      // Muioo nodes
      dstManager->AddNode("TMuiHitO");

      // Mutoo nodes
      dstManager->AddNode("TMutHit");

      dstManager->AddNode("PHMuoTracksOO");

      // From EVA node
      dstManager->AddNode("header");
      dstManager->AddNode("fkin");
      dstManager->AddNode("primary");
      dstManager->AddNode("pythia");

      // PHGlobal
      dstManager->AddNode("PHGlobal");
      dstManager->AddNode("PHGlobal_MUON");

      se->registerOutputManager(dstManager);
    }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  // Verify that libraries have been loaded.
  gSystem->ListLibraries();

  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllPisaInputManager *inMan = new Fun4AllPisaInputManager("PisaIn");
  se->registerInputManager(inMan);
  se->fileopen(inMan->Name(), inputfile);
  se->run(nEvents);
  se->End();

  cout << "Completed reconstruction." << endl;
}
