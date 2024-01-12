// $Id: Fun4FVTX_RecoPRDF.C,v 1.19 2014/03/07 16:21:19 jinhuang Exp $

/*!
prdf analysis loop for real data
creates a reconstructed DST, a NanoDST and a picoDST
*/

void Fun4FVTX_RecoPRDF(int nEvents = 100,
//    char *input_file = "file.prdf",
    char *input_file = "./EVENTDATA_P00-0000375372-0041.PRDFF",
		       char *ndst_file = "ndst_out.root",
		       char *filtered_prdf = "dimuon.prdf",
		       char *pdst_file_single = "singlemuon_pdst.root"
		       )
  
{
  bool is_pp  = false;
  bool has_vtx = true;
  bool save_dst = false;
  bool save_evtvtx = false;

  // load libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco" );
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");
  gSystem->Load("libfvtx_subsysreco.so");
  gSystem->Load("librecal.so");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", has_vtx);
  rc->set_IntFlag( "RpcGeomType", 3 );//NEEDED for run 12+ RPC geometry...

//  TMutExtVtx::get().set_verbosity( MUTOO::SOME );
//  TMutExtVtx::get().set_vertex_name("SVX_PRECISE");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  se->registerSubsystem( new HeadReco() );
  se->registerSubsystem( new SyncReco() );
  se->registerSubsystem( new TrigReco() );

  //set BBC-vtx resolution
  BbcReco *bbc_reco = new BbcReco();
  bbc_reco->setBbcVtxError(0.5);
  se->registerSubsystem( bbc_reco ); 

  if (is_pp)
    {
      // module which provides multiple vertices from BBC, necessary only in 510 Gev
      gSystem->Load( "libBbcMultipleVtx.so" );
      se->registerSubsystem( new BbcMultipleVtxReco() );
    }

  se->registerSubsystem( new ZdcReco() );
  se->registerSubsystem( new T0Reco() );
  se->registerSubsystem( new VtxReco() ); // makes VtxOut object

  if (has_vtx)
    {
       // SVX reconstruction
          SvxParManager *svxpar = new SvxParManager();
          svxpar->Verbosity(0);
          svxpar->set_UseStripThresholdDatbase(true);
          //  VTX Run12 alignment - Preview from Mike M. - V7
          svxpar->set_OffsetVtxToCnt(-0.124,-0.279,0.0); 
          svxpar->set_OffsetEastToWest(0.0451,0.0119,0.0030); 
          svxpar->set_BeamCenter(0.1653,-0.1353); 
          svxpar->set_ReadGeoParFromFile(true);
          svxpar->set_GeometryFileName(
              "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/svxPISA.par_v13_Dec2013_mccumber");
          se->registerSubsystem(svxpar);
         
          SvxDecode *svxdecode = new SvxDecode();
          svxdecode->Verbosity(0);
          svxdecode->includePixel(true);
          svxdecode->includeStripixel(true);
          svxdecode->setAdcOffset(24);
          svxdecode->setAdcCutoff(-24);
          se->registerSubsystem(svxdecode);
         
	  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
	  svxhotdead->Verbosity(0);
	  se->registerSubsystem(svxhotdead);
         
          SvxReco *svxrec = new SvxReco();
          svxrec->Verbosity(0);
          // svxrec->Load_ThresholdFile("threshold.h");
          //svxrec->set_UseStripThresholdDatbase(true);
          svxrec->set_StripixelAdcSumThreshold(0);
          se->registerSubsystem(svxrec);
         
          SvxPriVertexSeedFinder *svxvtxseedfinder = new SvxPriVertexSeedFinder();
          svxvtxseedfinder->Verbosity(0);
          se->registerSubsystem(svxvtxseedfinder);
         
          SvxPriVertexSeedFinder *vtxseedw = new SvxPriVertexSeedFinder("SVXPRIVERTEXFINDERW",1);
          se->registerSubsystem(vtxseedw);
         
          SvxPriVertexSeedFinder *vtxseede = new SvxPriVertexSeedFinder("SVXPRIVERTEXFINDERE",2);
          se->registerSubsystem(vtxseede);
         
          SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
          svxstandalone->Verbosity(0);
          svxstandalone->setVertexRecoFlag(2);
          //svxstandalone->setWindowScale(10);
          se->registerSubsystem(svxstandalone);
         
          SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
          svxprimvtxfinder->Verbosity(0);
          se->registerSubsystem(svxprimvtxfinder);
    }

  // muon prdf unpacker 
  MuonUnpackPRDF* muon_unpack_prdf( new MuonUnpackPRDF() );
  muon_unpack_prdf->Verbosity( 1 );
  se->registerSubsystem( muon_unpack_prdf ); 


  // TFvtxGlobalParCntrl should be loaded before any FVTX module
  TFvtxGlobalParCntrl::set_bool_par("use_svx", has_vtx);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", is_pp);


  // FVTX reconstruction
  FvtxUnpackPRDF *Fvtx_unpack = new FvtxUnpackPRDF();
  se->registerSubsystem(Fvtx_unpack);

  FvtxReco* fvtxreco = new FvtxReco();
  se->registerSubsystem(fvtxreco);

  // primary vertex from FVTX
  FvtxPrimVertex* fvtxprimvtx = new FvtxPrimVertex();
  se->registerSubsystem(fvtxprimvtx);
  
  // mutoo reconstruction
  se->registerSubsystem( new MuiooReco() );
  se->registerSubsystem( new MuonDev() );

  //RPC
  gSystem->Load( "librpc_subsysreco" );
  se->registerSubsystem( new RpcUnpackPRDF());
  se->registerSubsystem( new RpcReco());

  // FVTX-MuTr matching module
  se->registerSubsystem( new FvtxRecoWithMut());

  // New feature. Directly override default FVTX parameters
  // https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php?title=FVTX/Use_Non-Default_Reco_Parameters_in_Fun4All_Macros
  // e.g. following line change the FVTX-MuTr track matching plane to z = 50cm
  // Note: it have to run after  se->registerSubsystem( <Reco Module >);
  //  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")
  //      ->set_z_reference(50);


  // Melynda's evaluator
  //  FvtxEval* fvtxeval = new FvtxEval("FvtxEval",eval_file);
  //  se->registerSubsystem(fvtxeval);

  // QA histograms
  //  gSystem->Load("libdstqa_muons.so");
  //  se->registerSubsystem( new QAMui() );  // unstable
  //  se->registerSubsystem( new QAMut() );
  //  se->registerSubsystem( new QAFvtx() );

  se->registerSubsystem( new GlobalReco() );
  se->registerSubsystem( new GlobalReco_muons() );

  MasterRecalibratorManager *mr = new MasterRecalibratorManager();
  se->registerSubsystem(mr);

  // for embedding purposes
  if (save_evtvtx){
    MuonAnaTuples* ana_tuple = new MuonAnaTuples( "MUONANATUPLES" );
    se->registerSubsystem( ana_tuple );
    ana_tuple->set_event_filename( "event_vtx.txt");
    ana_tuple->set_flags( MuonAnaTuples::VERTEX | MuonAnaTuples::CENTRALITY);// | MuonAnaTuples::REACTION_PLANE);
  }
 
  // muon nanoDST module
  gSystem->Load( "libMWGOO" ); 
  MWGInclusiveNanoCutsv2 * nDST_cut = new MWGInclusiveNanoCutsv2() ;
  nDST_cut -> set_dodimu(false);
  SubsysReco *mwg = new MWGFvtxReco( nDST_cut );
  se->registerSubsystem(mwg);

  // PRDF filter
  // for options, see $OFFLINE_MAIN/include/MuonTrigFilter.h
  //  MuonTrigFilter * filter_dimuon = new MuonTrigFilter("DIMUON", 
  //						      MuonTrigFilter::RECO_DIMUON, MuonTrigFilter::DISCARD_EVENT);
  //  se->registerSubsystem(filter_dimuon);
  //  filter_dimuon->set_mass_min(1.0);

  // module which counts tracklets and clusters withing 8 cone ranges
  if (is_pp)
    {
      FvtxConeTracklets* fvtxcone = new FvtxConeTracklets();
      se->registerSubsystem(fvtxcone);
    }

  // File Managers
  if (save_dst)
    {
      Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",  dst_file);

      dstManager->AddNode("Sync");
      dstManager->AddNode("TrigLvl1");
      dstManager->AddNode("L2Decision");
      dstManager->AddNode("Lvl2OutArray");
      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");
      dstManager->AddNode("VtxOut");
      //dstManager->AddNode("BbcOut");
      //dstManager->AddNode("BbcRaw");
      //dstManager->AddNode("ZdcOut");
      //dstManager->AddNode("ZdcRaw");

      //FVTX nodes
      dstManager->AddNode("TFvtxHit");
      //dstManager->AddNode("TFvtxCoord");
      //dstManager->AddNode("TFvtxTrk");
      
      // Muioo nodes
      dstManager->AddNode("TMuiHitO");
      //dstManager->AddNode("TMuiRoadO");
      //dstManager->AddNode("TMuiClusterO");
      //dstManager->AddNode("TMui1DRoadO");
      //dstManager->AddNode("TMuiPseudoBLTO");
      //dstManager->AddNode("TMuiPseudoLL1");
      
      // Mutoo nodes
      dstManager->AddNode("TMutHit");
      //dstManager->AddNode("TMutMuiRoad");
      //dstManager->AddNode("TMutClus");
      //dstManager->AddNode("TMutCoord");
      //dstManager->AddNode("TMutGapCoord");
      //dstManager->AddNode("TMutStub");
      //dstManager->AddNode("TMutTrk");
      //dstManager->AddNode("TMutVtx");

      // PHGlobal
      dstManager->AddNode("PHGlobal");
      dstManager->AddNode("PHGlobal_MUON");
      
      se->registerOutputManager(dstManager);
    }

  // picoDST modules
  //  gSystem->Load("libpicodst_object");
  //  mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
  //  se->registerSubsystem(msngl);
  //  msngl->set_bbcz_cut(100);
  //  msngl->set_pz_cut(2.0);
  //  msngl->set_make_reassociation(true);

  //  Fun4AllOutputManager *outsingle = new Fun4AllDstOutputManager("Outsingle",pdst_file_single);
  //  outsingle->AddNode("Sync");
  //  outsingle->AddNode("SingleMuonContainer");
  //  outsingle->AddNode("TrigLvl1");
  //  outsingle->AddEventSelector("mFillSingleMuonContainer");
  //  se->registerOutputManager(outsingle);

  Fun4AllDstOutputManager *nanodstManager  = new Fun4AllDstOutputManager("nanoDSTOUT",  ndst_file);
  nanodstManager->AddNode("Sync");
  nanodstManager->AddNode("RunHeader");
  nanodstManager->AddNode("EventHeader");
  nanodstManager->AddNode("TrigLvl1");
  nanodstManager->AddNode("PHMuoTracksOO");
  nanodstManager->AddNode("VtxOut");
  nanodstManager->AddNode("PHGlobal");
  nanodstManager->AddNode("PHGlobal_MUON");
  nanodstManager->AddNode("TFvtxCompactTrk");
  se->registerOutputManager(nanodstManager);

  //  Fun4AllEventOutputManager *prdf_manager = new Fun4AllEventOutputManager("DIMUON_PRDF", filtered_prdf);
  //  prdf_manager->AddEventSelector("DIMUON");
  //  se->registerOutputManager(prdf_manager);

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(input_file); // for one file
  //in->AddListFile(input_file); // for a list of files
  se->registerInputManager(in);

  se->run(nEvents);

  se->End();

  cout << "Completed reconstruction." << endl;
}
