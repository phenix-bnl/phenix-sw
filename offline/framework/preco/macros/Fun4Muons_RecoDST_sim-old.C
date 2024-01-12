// $Id: Fun4Muons_RecoDST_sim-old.C,v 1.4 2010/09/03 19:18:04 hpereira Exp $
/*!
  mutoo dst analysis loop for mutoo slowsim simulated dst, possibly embeded
  into either simulated or real data dst
*/
void Fun4Muons_RecoDST_sim_old(
  int nEvents = 100,
  char *signalfile = "signal.root",
  char *backgroundfile = "background.root",
  char *dstfile   = "dst_out.root",
  char *prdffile  = "data_out.prdf",
  char *ndstfile  = "ndst_out.root",
  char *pdstfile  = "pdst_out.root"
)
{

  // dump configuration
  cout << endl;
  cout << "Fun4Muons_RecoDST_sim-old" << endl;
  if( signalfile ) cout << "Fun4Muons_RecoDST_sim-old - signalfile : " << signalfile << endl;
  if( backgroundfile ) cout << "Fun4Muons_RecoDST_sim-old - backgroundfile : " << backgroundfile << endl;
  if( dstfile ) cout << "Fun4Muons_RecoDST_sim-old - dstfile : " << dstfile << endl;
  if( prdffile ) cout << "Fun4Muons_RecoDST_sim-old - prdffile : " << prdffile << endl;
  if( ndstfile ) cout << "Fun4Muons_RecoDST_sim-old - ndstfile : " << ndstfile << endl;
  if( pdstfile ) cout << "Fun4Muons_RecoDST_sim-old - pdstfile : " << pdstfile << endl;

  // dst/ndst/pdst output
  bool write_dst = true;
  bool write_ndst = true;
  bool write_pdst = true;

  // MUTOO evaluator
  bool do_evaluation = true;

  // code profiler
  bool do_profile = false;

  // perfect detector geometry (no dead channels/HV; 100% muid efficiency)
  bool use_perfect_detector = false;

  // use RPC software
  bool use_RPC = true;

  // use FVTX software
  bool use_FVTX = true;
  bool FVTX_fastsim = false;  // If true, project muon hits to FVTX and create hits
  bool FVTX_mcreco = false;   // Take MC hits, optionally smear, make tracks and fit
  bool FVTX_realreco = true;  // Take FVTX clusters, make coords and tracks and fit

  /*
    when this flag is true, the mutr/muid hit maps merged after the embedding
    are packed again to a "merged" simulated PRDF
  */
  bool write_merged_prdf = false;

  // load libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs" );
  gSystem->Load("libcteval" );

  if( do_evaluation )
  {
    // evaluation requires some extra reconstruction modules
    // from muon_subsysreco
    gSystem->Load("libmuon_subsysreco");
  }

  gSystem->Load("libMWG_interface");

  if( do_profile ) {
    gSystem->Load("libjprof");
    prof *Pr = new prof;
  }

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();

  rc->set_IntFlag("MUONFUN4SIM",1);
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);

  // database control initialization
  if( use_perfect_detector ) {

    // tell muid tube efficiency must be set to 1
    rc->set_DoubleFlag("MUIOO_TUBE_EFF",1.0);

    // make perfect mutr detector
    TMutDatabaseCntrl::set_database_access("disable_HV",false);
    TMutDatabaseCntrl::set_database_access("attenuated_channels",false);
    TMutDatabaseCntrl::set_database_access("dead_channels",false);

  } else {

    /*
      to have muid inneficiencies turned on you need to have local copies of
      tube_eff_north_default.txt
      tube_eff_south_default.txt
    */

    /*
      to have mutr HV mask turned on, you need to have the following set to true
      and a local copy of mut.disabledAnodes.dat
    */
    TMutDatabaseCntrl::set_database_access("disable_HV",true);
    TMutDatabaseCntrl::set_database_access("use_local_dead_HV_file",true);

    // this tells that the mutr dead wires should be read from the database
    TMutDatabaseCntrl::set_database_access("use_local_dead_wire_file",false);

    // this enable use of attenuateed/dead channels from database
    TMutDatabaseCntrl::set_database_access("attenuated_channels",true);
    TMutDatabaseCntrl::set_database_access("dead_channels",true);

  }

  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN, TMutDatabaseCntrl::NONE );
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN, TMutDatabaseCntrl::NONE );
  TMutDatabaseCntrl::print();

  mMfmMT::setMapFileFlag(4);
  mMfmMT::setMapFileScale(1.0);
  MuonUtil::set_check_mapfile_scale( false );
  TMutChargeCorrection::set_do_correction( false );

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  // counter
  se->registerSubsystem( new MuonCounter() );

  // this part should no be necessary unless
  // you run on an 'old' simulated DST (for which VtxSimreco did not run in Fun4Muons_PISA
  bool use_vtx_sim_reco = false;
  if( use_vtx_sim_reco )
  {
    gSystem->Load("libsimreco_base");
    VtxSimreco* vtx_reco = new VtxSimreco();
    vtx_reco->SmearZ( true );
    vtx_reco->UseXY( false );
    vtx_reco->OverwriteBBC( false );
    vtx_reco->ZVertexSigma( 0.5 );
    se->registerSubsystem( vtx_reco );
  }

  // Muon ext vtx verbosity
  // this allows to print which vertex is used and its value
  TMutExtVtx::get().set_verbosity( MUTOO::SOME );

  // Mutr/Muid unpacking
  MuonUnpackSim* muon_unpack = new MuonUnpackSim();
  if( backgroundfile ) muon_unpack->SetMode( MuonUnpackSim::MC_SIGNAL_REAL_BG );
  else muon_unpack->SetMode( MuonUnpackSim::MC_SIGNAL_NO_BG );

  se->registerSubsystem( muon_unpack );

  // RPC unpacking/reconstruction
  if( use_RPC )
  {
    gSystem->Load("librpc_subsysreco");
    if( backgroundfile ) se->registerSubsystem( new RpcUnpackSim( "RPCUNPACKSIM", RpcUnpackSim::MC_SIGNAL_REAL_BG ) );
    else  se->registerSubsystem( new RpcUnpackSim( "RPCUNPACKSIM", RpcUnpackSim::MC_SIGNAL_NO_BG ) );
    se->registerSubsystem( new RpcReco() );
  }

  if( use_FVTX )
  {
    gSystem->Load("libfvtx_subsysreco");
    if( FVTX_fastsim ) {
      se->registerSubsystem( new FvtxFastSim() );
    } else if( FVTX_mcreco ) {
      se->registerSubsystem( new FvtxUnpackSim() );
      se->registerSubsystem( new FvtxRecoMC() );
    } else if( FVTX_realreco ) {
      se->registerSubsystem( new FvtxUnpackSim() );
      se->registerSubsystem( new FvtxReco() );
    }
    if( do_evaluation ) se->registerSubsystem( new FvtxMCEval() );
  }

  // Mutr/Muid reconstruction
  se->registerSubsystem( new MuiooReco() );
  se->registerSubsystem( new MuonDev() );

  /*
    global Reco must be run after the reconstruction
    but before the evalutations for all nodes/ntuples to be filled
    properly
  */
  se->registerSubsystem( new GlobalReco() );
  se->registerSubsystem( new GlobalReco_muons() );

  // official muon evaluation modules
  if( do_evaluation ) {
    se->registerSubsystem( new MuonAnaTuples() );
    se->registerSubsystem( new MuonEval() );
  }

  // prdf packer
  if( prdffile && write_merged_prdf )
  se->registerSubsystem( new MuonPackPRDF() );

  ///////////////////////////////////////////
  // Input manager
  ///////////////////////////////////////////
  Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager( "SIGNAL_IM", "SIGNAL" );
  se->registerInputManager( signal );
  se->fileopen( signal->Name(), signalfile );

  // try load simulated background, if requested
  if(backgroundfile )
  {

    Fun4AllInputManager *background = new Fun4AllNoSyncDstInputManager( "BACKGROUND_IM", "BACKGROUND" );
    se->registerInputManager(background);
    se->fileopen( background->Name(),backgroundfile );

  }

  /*
    try load background also as a head dst, to get global detectors
    this should not interfere with mutoo nodes (even if duplicated) since they
    are overwritten by the PHMapManager
  */
  if( backgroundfile )
  {

    cout << "Fun4Muons_RecoDST_sim - reoppening BACKGROUND under DST node to get additionnal nodes copied.\n";
    Fun4AllInputManager *background = new Fun4AllNoSyncDstInputManager( "BACKGROUND_DST_IM", "DST" );
    se->registerInputManager(background);
    se->fileopen( background->Name(),backgroundfile );

  } else if( signalfile ) {

    cout << "Fun4Muons_RecoDST_sim - reoppening SIGNAL under DST node to get additionnal nodes copied.\n";
    Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager( "SIGNAL_DST_IM", "DST" );
    se->registerInputManager( signal );
    se->fileopen( signal->Name(), signalfile );

  }

  ///////////////////////////////////////////
  // Output manager
  ///////////////////////////////////////////

  // dst output manager
  if( dstfile && write_dst )
  {

    Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",dstfile);
    se->registerOutputManager(dstManager);

    // Header and vertex nodes
    dstManager->AddNode("RunHeader");
    dstManager->AddNode("EventHeader");
    dstManager->AddNode("VtxOut");
    dstManager->AddNode("BbcOut");
    dstManager->AddNode("BbcRaw");
    dstManager->AddNode("ZdcOut");
    dstManager->AddNode("ZdcRaw");
    dstManager->AddNode("TrigLvl1");

    dstManager->AddNode("TMuiPseudoLL1");

    // muioo nodes
    dstManager->AddNode("TMCPrimary");
    dstManager->AddNode("TMuiMCHitO");
    dstManager->AddNode("TMuiHitO");
    dstManager->AddNode("TMuiClusterO");
    dstManager->AddNode("TMui1DRoadO");
    dstManager->AddNode("TMuiRoadO");
    dstManager->AddNode("TMuiPseudoBLTO");
    dstManager->AddNode("TMuiPseudoLL1");

    // mutoo nodes
    dstManager->AddNode("TMutMCHit");
    dstManager->AddNode("TMutMCTrk");
    dstManager->AddNode("TMutHit");
    dstManager->AddNode("TMutClus");
    dstManager->AddNode("TMutCoord");
    dstManager->AddNode("TMutGapCoord");
    dstManager->AddNode("TMutStub");
    dstManager->AddNode("TMutTrk");
    dstManager->AddNode("TMutVtx");

    // rpc nodes
    if( use_RPC )
    {
      dstManager->AddNode("TRpcMCHit");
      dstManager->AddNode("TRpcHit");
      dstManager->AddNode("TRpcClus");
      dstManager->AddNode("TRpcCoord");
      dstManager->AddNode("TRpcTrk");
    }

    // Fvtx nodes
    if( use_FVTX )
    {
      dstManager->AddNode( "TFvtxMCHit" );
    }

    // From EVA node
    dstManager->AddNode("header");
    dstManager->AddNode("fkin");
    dstManager->AddNode("primary");
    dstManager->AddNode("pythia");
  }

  /*
    nanodst setup.
    ndst module must be run when either nano or picodst is required
  */
  if( (ndstfile && write_ndst) || (pdstfile && write_pdst) )
  {

    // register nanoDST subsystem
    gSystem->Load("libMWGOO");
    PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
    se->registerSubsystem(new MWGOOReco(MWGcuts));

  }

  // ndst output manager
  if( ndstfile && write_ndst )
  {

    Fun4AllDstOutputManager *ndstManager = new Fun4AllDstOutputManager( "MWGOUT",ndstfile );
    se->registerOutputManager(ndstManager);

    ndstManager->AddNode("RunHeader");
    ndstManager->AddNode("EventHeader");
    ndstManager->AddNode("header");
    ndstManager->AddNode("BbcOut");
    ndstManager->AddNode("PHGlobal");
    ndstManager->AddNode("PHGlobal_MUON");
    ndstManager->AddNode("PHMuoTracksOO");
    ndstManager->AddNode("TrigLvl1");
    ndstManager->AddNode("TMuiPseudoBLTO");
    ndstManager->AddNode("TMuiPseudoLL1");
    ndstManager->AddNode("VtxOut");

  }

  // picodst output manager
  if( pdstfile && write_pdst )
  {

    gSystem->Load("libMWGpico");

    // register picoDST subsystem
    /*
      note on the vertex loading:
      if you want the BBC vertex to be loaded instead of the pisa MC vertex,
      use MWGpico *picoDST = new MWGpico();
      instead of the following
    */
    MWGpico *picoDST = new MWGpico( "simu" );
    picoDST->MakePico( MWGpico::DIMUONSOO, pdstfile );
    picoDST->MakePico( MWGpico::HISTOGRAMS, pdstfile );
    picoDST->InitCuts("nocuts");
    picoDST->PrintCuts();

    se->registerSubsystem(picoDST);

  }

  // prdf output
  if( prdffile && write_merged_prdf )
  {

    Fun4AllPrdfOutputManager *prdf_io  = new Fun4AllPrdfOutputManager("PRDFOUT",prdffile);
    prdf_io->InitPrdfNode( se->topNode() );
    se->registerOutputManager(prdf_io);

  }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  MuonUtil::dump_process_time();
  se->run(nEvents);
  se->End();
  MuonUtil::dump_process_time();
  cout << "Completed reconstruction." << endl;

}
