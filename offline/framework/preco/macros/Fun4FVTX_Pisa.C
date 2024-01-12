// $Id: Fun4FVTX_Pisa.C,v 1.16 2014/05/02 18:22:33 brooks Exp $
/*!
  Response code for simulation. Read in the pisa file and create hits,
  creates an unreconstructed DST
*/

void Fun4FVTX_Pisa(
  int nEvents = 20,
  char *inputfile = "PISAEvent.root",
  const char *evtgen = "phpythia.root",
  char *dstfile   = "dst_out.root",
  char *ndstfile   = "ndst_out.root",
  char *ana_file = "muon_ana_ntuples.root",
  const char *singlepdstout = "singlemuon_pdst.root",
  const char *dimuonpdstout = "dimuon_pdst.root",
  int run_number =  397401 //372412 //398120 //372412 //365000 //372412// 360502 - this run number will give perfect dead map
)

{
  // flags
  bool use_lvl2 = false;
  bool use_fvtx = true;
  bool write_ndst = false;
  bool write_pdst = true;
  bool write_qa = false;
  bool read_pythia = true;
  bool use_svx = true;
  bool use_rpc = false;
  bool use_eval = false;
  bool is_pp = true;

  const char* qafile = "qaOut.root";

 // load libraries
  gSystem->Load("libPHGeant" );
  gSystem->Load("libfun4all");
  gSystem->Load("libsimreco");
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
  TMutDatabaseCntrl::set_database_access("disable_HV",true);
  TMutDatabaseCntrl::set_database_access("use_local_landau_parameters_file", true );
  TMutDatabaseCntrl::set_filename("use_local_landau_parameters_file",
                                  "landau_parameters.txt");
  TMutDatabaseCntrl::set_database_access("attenuated_channels",true);
  TMutDatabaseCntrl::set_database_access("dead_channels",true);

  bool use_local_hv_files = true;
  if( use_local_hv_files )
    {
      // generate hv file from run number
      std::stringstream hv_file;
      hv_file << "/phenix/spin/phnxsp01/keyaaron/Run13MuTrHV/mut.disabledAnodes.dat_run" << run_number;
      TMutDatabaseCntrl::set_database_access("use_local_dead_HV_file",true);
      TMutDatabaseCntrl::set_filename("use_local_dead_HV_file", hv_file.str().c_str());      
    } 
  else TMutDatabaseCntrl::set_database_access("use_local_dead_HV_file",false);

  /*
    disable check of mapFileScale since its decided by pisa,
    not by the run number.
  */
  mMfmMT::setMapFileFlag( mMfmMT::MAP_3D_PLUS_PLUS );
  mMfmMT::setMapFileScale( 1.0 );
  MuonUtil::set_check_mapfile_scale( false );

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", use_svx );
  rc->set_IntFlag("SIMVERTEXFLAG",2);
  rc->set_FloatFlag("SIMZ0VERTEX",0);
  rc->set_FloatFlag("SIMT0VERTEX",0);
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",0.00);
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",0);
  rc->set_IntFlag("SIMULATIONFLAG",2);
  rc->set_IntFlag("EMBEDFLAG",0);
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);
  rc->set_IntFlag("RUNNUMBER", run_number);

  if (use_lvl2) 
    {
      // Set Lvl2 flags
      rc->set_IntFlag("LVL2_REAL_DATA",1);
      rc->set_IntFlag("LVL2_YEAR",4);
      rc->set_IntFlag("FORCE_LVL2",1);
      rc->set_IntFlag("LVL2_USE_ASCII_DB",1);
      rc->set_CharFlag("LVL2_DB_DIR","/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN4_REAL");
      rc->set_CharFlag("Run2Lvl2AlgoName", "");
    }

  // Read the FVTX dead channel map and geometry from the database:
  TFvtxGlobalParCntrl::set_bool_par("is_sim",true);
  TFvtxGlobalParCntrl::set_bool_par("use_svx",use_svx);
  TFvtxGlobalParCntrl::set_bool_par("deadmap_use_calibration_database", true); 
  TFvtxGlobalParCntrl::set_bool_par("geom_use_calibration_database", false); 
  //TFvtxDatabaseCntrl::set_filename("geom_root_file_path",
  //   "/phenix/u/workarea/brooks/simulation/pisa2000/wrk/");
  //TFvtxDatabaseCntrl::set_filename("geom_root_file_name", "fvtxgeom_v7.root");
  TFvtxGlobalParCntrl::set_string_par("geom_root_file_path",
				      "/phenix/u/jinhuang/work/FVTX/miliped_work/millipede/");
  TFvtxGlobalParCntrl::set_string_par("geom_root_file_name", "fvtx_geometry.root");
  
  //TFvtxDatabaseCntrl::set_flag("deadmap_auto_load", true); // don't auto load again
  //TFvtxDeadMap map;
  //map.dbGetAll(run_number, /*bool is_sim*/false); // fetch the map from database
  //map.apply_dead_maps();  // apply dead map immediately
  
  //See what happens if we change the b-field scale:
  //mMfmMT::setMapFileScale( 1.00 );
  
  // mutoo vertex source configuration
  TMutExtVtx::get().set_verbosity( MUTOO::ALOT );
  TMutExtVtx::get().set_priorities( "SIM", 0 );
  
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  // run header and trigger setting
  se->registerSubsystem( new HeadSimreco() );
  //    se->registerSubsystem( new TrigSimreco() );

  // vertex simulation
  // puts the vertex from the pisa header node into vtxOut object
  gSystem->Load( "libsimreco_base.so" );
  VtxSimreco* vtx_reco = new VtxSimreco();
  vtx_reco->SmearZ( false );
  vtx_reco->UseXY( true );
  vtx_reco->OverwriteBBC( true );
  //  vtx_reco->XVertexSigma( 0.01 ); // FVTX resolution of 100 microns
  //  vtx_reco->YVertexSigma( 0.01 );
  //  vtx_reco->ZVertexSigma( 0.1 ); 
  se->registerSubsystem( vtx_reco );

  // Counter
  MuonCounter* mc = new MuonCounter();
  mc->set_event_dump(1000);
  se->registerSubsystem( mc );

  // global detectors subsystem
  //se->registerSubsystem( new HeadReco() );
  //se->registerSubsystem( new TrigReco( ));
  //se->registerSubsystem( new BbcReco() );
  //se->registerSubsystem( new ZdcReco() );

  // level2 selection subsystems
  if (use_lvl2) {
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

  // SVX reconstruction"
  if ( use_svx )
    {
      SvxParManager *svxpar = new SvxParManager();
      svxpar->Verbosity(0);
      //svxpar->set_BeamCenter(0.1653,-0.1353);
      //svxpar->set_OffsetVtxToCnt(-0.124,-0.279,0.0);
      //svxpar->set_OffsetEastToWest(0.0451,0.0119,0.0030);
      svxpar->set_BeamCenter(0.,0.);
      svxpar->set_OffsetVtxToCnt(0.0, 0.0, 0.0);
      svxpar->set_OffsetEastToWest(0.0, 0.0, 0.0);
      svxpar->set_ReadGeoParFromFile(1);
      svxpar->set_GeometryFileName("svxPISA.par.ideal");
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
    }
  
  // muon prdf unpacker
  MuonUnpackPisa* muon_unpack_pisa( new MuonUnpackPisa() );
  muon_unpack_pisa->Verbosity( 1 );
  //Option to skip the offline zero-suppression:
  muon_unpack_pisa->set_flag(MuonUnpackPisa::DO_RESPONSE,1);
  //  muon_unpack_pisa->set_flag(MuonUnpackPisa::NO_ZERO_SUP,1);
  //  muon_unpack_pisa->set_flag(MuonUnpackPisa::ADD_NOISE,1);
  se->registerSubsystem( muon_unpack_pisa );

  // mutoo reconstruction
  se->registerSubsystem( new MuiooReco() );
  se->registerSubsystem( new MuonDev() );

  if (use_rpc)
    {
      //RPC
      gSystem->Load( "librpc_subsysreco" );
      se->registerSubsystem( new RpcUnpackPRDF());
      se->registerSubsystem( new RpcReco());
    }

  // fvtx prdf unpacker
  if (use_fvtx)
    {
      FvtxUnpackPisa *fvtx_unpack = new FvtxUnpackPisa();
      fvtx_unpack->set_do_response( true );
      fvtx_unpack->Verbosity(0);
      se->registerSubsystem( fvtx_unpack );
      
      FvtxReco* fvtxreco = new FvtxReco();
      //fvtxreco->set_do_mutkalfiteval(true);         // Adds VTX hits to tracking
      //fvtxreco->run_after_burner(false);         // Adds VTX hits to tracking
      //fvtxreco->set_use_svx_cluster(true);     // Uses smeared PISA hits if false
      fvtxreco->set_do_mutr_matching(false);     // Match FVTX-MuTr tracks and refit or not
      //fvtxreco->set_finder(3);                  // Use Hough track finder (default)
      se->registerSubsystem(fvtxreco);
      
      if ( is_pp )
	{
	  TMutNode<mFvtxFindTrackPar>::find_node(se->topNode(),"mFvtxFindTrackPar")->set_allowTwoHitTracks(true);
	  TMutNode<mFvtxFindTrackPar>::find_node(se->topNode(),"mFvtxFindTrackPar")->set_filterTwoHitTracks(true);      
	}
    
      // Perform FVTX-Mutr track matching and refit track:
      se->registerSubsystem( new FvtxRecoWithMut() );
    }
  
  if ( use_eval )
    {
      se->registerSubsystem( new MuonAnaTuples() );
      se->registerSubsystem( new MuonEval() );
      
      if (use_fvtx)
	{
	  FvtxEval* fvtxeval = new FvtxEval("FvtxEval","fvtx_eval_pisa.root");
	  se->registerSubsystem(fvtxeval);
	  FvtxMCEval* fvtxeval_mc = new FvtxMCEval("FvtxMCEval","fvtx_mc_eval_pisa.root");
	  se->registerSubsystem(fvtxeval_mc);
	}
    }

  if (write_qa)
    {
      // QA histograms
      gSystem->Load("libdstqa_muons.so");
      //  se->registerSubsystem( new QAMui() );  // unstable
      se->registerSubsystem( new QAMut() );
      se->registerSubsystem( new QAFvtx() );
    }

    // picoDST
    if( singlepdstout && write_pdst )
    {
        //      se->registerSubsystem( new MpcReco() );
        //      gSystem->Load("librxnp_subsysreco.so");
        //      se->registerSubsystem( new RxnpReco() );
        //      se->registerSubsystem( new RpSumXYReco() ); // recalibrator and rp doesn't work together!

        // global Reco
        se->registerSubsystem( new GlobalReco() );

        // MWG
        gSystem->Load("libMWGOO.so");
        PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
        se->registerSubsystem(new MWGFvtxReco(MWGcuts));

      // module which counts tracklets and clusters withing 8 cone ranges
      // FvtxConeTracklets* fvtxcone = new FvtxConeTracklets();
      // fvtxcone->set_make_eval(false);
      // se->registerSubsystem(fvtxcone);     

        gSystem->Load("libpicodst_object.so");
	mFillMCSingleMuonContainer* mcsngl = new mFillMCSingleMuonContainer();
	se->registerSubsystem(mcsngl);	
	
	mFillMCDiMuonContainer* mcdi = new mFillMCDiMuonContainer();
	se->registerSubsystem(mcdi);	

	mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
	msngl->set_is_sim(true);
	se->registerSubsystem(msngl);
	msngl->set_bbcz_cut(100);
	
	mFillDiMuonContainer* mdi = new mFillDiMuonContainer(false); // do not make mixed events
	mdi->set_is_sim(true);
	se->registerSubsystem(mdi);
	mdi->set_mass_cut(0.5);

        if ( write_ndst )
          {
            Fun4AllOutputManager *outndst = new Fun4AllDstOutputManager("Outndst", ndstfile);
            outndst->AddNode("Sync");
            outndst->AddNode("TrigLvl1");
            outndst->AddNode("VtxOut");
            outndst->AddNode("PHGlobal");
            outndst->AddNode("PHPythiaHeader");
            outndst->AddNode("PHPythia");
            outndst->AddNode("PHMuoTracksOO");
            se->registerOutputManager(outndst);
          }

        Fun4AllOutputManager *outsmu = new Fun4AllDstOutputManager("Outsmu",singlepdstout);
        outsmu->AddNode("Sync");
        outsmu->AddNode("SingleMuonContainer");
        outsmu->AddNode("VtxOut");
        outsmu->AddNode("PHGlobal");
        outsmu->AddNode("PHPythiaHeader");
        outsmu->AddNode("PHPythia");
        outsmu->AddEventSelector("mFillSingleMuonContainer");
        se->registerOutputManager(outsmu);

        Fun4AllOutputManager *outdimu = new Fun4AllDstOutputManager("Outdimu",dimuonpdstout);
        outdimu->AddNode("Sync");
        outdimu->AddNode("DiMuonContainer");
        outdimu->AddNode("PHPythiaHeader");
        outdimu->AddNode("PHPythia");
        outdimu->AddEventSelector("mFillDiMuonContainer");
        se->registerOutputManager(outdimu);
   }



  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////

  // dst
  if( dstfile ) {
    Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",  dstfile);

    dstManager->AddNode("Sync");
    dstManager->AddNode("RunHeader");
    dstManager->AddNode("EventHeader");
    dstManager->AddNode("PreviousEvent");
    dstManager->AddNode("TrigRunLvl1");
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
    dstManager->AddNode("TMuiMCHitO");

    // Mutoo nodes
    dstManager->AddNode("TMutHit");
    dstManager->AddNode("TMutMCHit");
    dstManager->AddNode("TMutMCTrk");

    // FVTX nodes
    dstManager->AddNode("TFvtxHit");
    dstManager->AddNode("TFvtxMCHit");
    dstManager->AddNode("TFvtxPisaHit");

    // SVX nodes
    dstManager->AddNode("McSingle");
    dstManager->AddNode("SvxPisaHit");
    dstManager->AddNode("SvxGhitList");
    dstManager->AddNode("SvxRawhitList");
    dstManager->AddNode("SvxGhitRawhitList");
    dstManager->AddNode("PHCentralTrack");
    dstManager->AddNode("SvxCentralTrackList");

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

  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllPisaInputManager *inMan = new Fun4AllPisaInputManager("PisaIn");
  se->registerInputManager(inMan);
  se->fileopen(inMan->Name(),inputfile);

  if ( read_pythia )
    {
      Fun4AllDstInputManager *ipythia = new Fun4AllNoSyncDstInputManager("DSTin2","DST");
      se->registerInputManager(ipythia);
      se->fileopen(ipythia->Name(),evtgen);
    }

  se->run(nEvents);
  se->End();

  if ( write_qa )
    {
      Fun4AllHistoManager *hm = se->getHistoManager("QA");
      if( hm ) hm->setOutfileName( qafile );
      else cout << "Fun4Muons_Pisa - unable to find QA histograms" << endl;
      se->dumpHistos();
    }

  cout << "Completed reconstruction." << endl;
}
