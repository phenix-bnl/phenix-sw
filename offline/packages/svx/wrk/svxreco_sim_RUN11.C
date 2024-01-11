/*
  INFORMATION OF FLAGS
  
  ///////////////////////////////////////
  ///  SvxParManager
  ///////////////////////////////////////

  =========================
  Geometrical configuration
  =========================

  - set_OffsetVtxToCnt(xoffset, yoffset, zoffset)
  Set the coordinate offset between VTX and central arm.
  If you don't call the function, the offset is obtained from database.

  - set_OffsetEastToWest(xoffset, yoffset, zoffset)
  Set the coordinate offset between west and east arms of VTX.
  If you don't call the function, the offset is obtained from database.
  
  - set_BeamCenter(beam_x, beam_y)
  Set rough beam center position
  If you don't call the function, the beam center is obtained from database.

  - set_ReadGeoParFromFile(flag)
  Set a flag to define how to get geometrical information of VTX.
    * flag  = 0 : get from database. (default)
    * flag != 0 : get from a file.

  - set_GeometryFileName(filename)
  Set name of file containing geometrical information of VTX.
  Default file name is "svxPISA.par"

  ================
  Hot/dead channel
  ================

  - set_ReadPixelMapFromFile(flag)
  Set a flag to define how to get information of hot/dead channels of pixel layers.
    * flag  = 0 : get from database. (default)
    * flag != 0 : get from a file.
  
  - set_ReadStripHotDeadFromFile(flag)
  Set a flag to define how to get information of hot/dead channels of pixel layers.
    * flag = true  : get from a file.
    * flag = false : get from database. (default)
   
  - set_PixelHotDeadChipFileName(filename);
  Set name of file containing information of hot/dead chips of pixel layers.
  Default file name is "pixelchipmap.txt"
  
  - set_PixelHotDeadPixelFileName(filename);
  Set name of file containing information of hot/dead pixels of pixel layers.
  Default file name is "pixelpixelmap.txt"
  
  - set_StripHotDeadHybridsFileName(filename);
  Set name of file containing information of hot/dead hybrids of stripixel layers.
  Default file name is "svxStripHybridsDeadMap.txt"

  - set_StripHotDeadFileName(filename);
  Set name of file containing information of hot/dead channels of stripixel layers.
  Default file name is "svxStripDeadMap.txt"


  ================
  Strip threshold
  ================

  - set_UseStripThresholdDatbase(flag)
  Set flag to define how to get stripixel threshold information.
    * flag = true  : get from database. (default)
    * flag = false : get from a file.

  - Load_ThresholdFile(filename)
  Set file name containing threshold information.
  No default, so you have to set the file name if you set
  useStripThresholdDatabase = false.


  ///////////////////////////////////////
  ///  SvxReco
  ///////////////////////////////////////
  
  - set_ThisIsSimulation()
  If you analyze simulation data, you should call this function.
  Otherwise, SvxGhitClusterList is not created.


  ///////////////////////////////////////
  ///  SvxStandAloneReco
  ///////////////////////////////////////

  - setVertexRecoFlag(flag)
  Set flag to define how to get collision vertex information which is used
  input of track search.
    * flag != 2 : x & y vertex positions are those set by 
                  SvxParManager::set_BeamCenter(), and z vertex position is 0.
    * flag  = 2 : vertex information is get from VtxOut node. (default)

  - setWindowScale(scale)
  Set scale factor of hit search window at track finding.
  Default is 1.

  - setProjectionFlag(flag)
  If you set false, the collision vertex is not used for the projection at
  hit search at track finding.
  Default is true.

  - setBbcChargeCut(cut)
  If BBC charge (north+south) is less than the "cut", search window parameter
  for low multiplicity events are used.
  Default value of the cut is 200.

  - setPPFlag(flag)
  If you analyze p+p collision events, please set flag=true.
  If you set flag=true, the collision vertex is not used for the projection at
  hit search at track finding (this can be done by setProjectionFlag(false).),
  and search window parameter for low multiplicity events are used.
  Default is false.

  - setSwapFlag(flag)
  If you want to evaluate fake background with hit position swap method,
  you have to set this flag.
    * flag = 1 : all hits on layer2 are converted to fake hits.
    * flag = 2 : all hits on layer3 are converted to fake hits.
    * flag = 3 : all hits on both layer2 and layer3 are converted to fake hits.

*/

void svxreco_sim_RUN11(
  Int_t nEvents = 100,
  char *filein="PISAEvent.root",
  char *dstout = "simDST.root",
  int run_number = 310380
)
///
/// INPUT
///   nEvents    : Number of events which you want to reconstruct.
///                If you set 0, all events are reconstructed.
///   filein     : input PISA file
///   dstout     : output file name
///   run_number : When you want to use database, you have to set proper
///                number here.
///
{
  // allow to disable muon simulations
  // they are enabled by default
  bool do_muon_arms = false;

  ///////////////////////////////////////////
  // Load Libraries
  //////////////////////////////////////////
  // gSystem->Load("libsvx");
  /// If you want to use a private svx library, put the pass the library here.
  /// Otherwise you don't have to load libsvx.so.
  /// It is loaded at libfun4all.so
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs");
  gSystem->Load("libsimreco");
  gSystem->Load("libcompactCNT.so");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();

  // 2 means PISA-To-DST
  rc->set_IntFlag("SIMULATIONFLAG",2);
  // disable embedding
  rc->set_IntFlag("EMBEDFLAG",0);
  // Reference run number used in 2007 Au+Au 200 GeV
  rc->set_IntFlag("RUNNUMBER",run_number);
  // Requested by EMCal
  rc->set_IntFlag("EVALUATIONFLAG", 1);
  // this should be moved to the Init method of TofSimreco
  rc->set_FloatFlag("TOFTIMINGRESOLUTION", 0.100);
  /*
  Flags to abort event if required number of GEANT hits is not present
  in the subsystem
  Defaults are all 0 except for the Drift Chamber
  default setting is 3 Drift Chamber wire plane hits
  */
  rc->set_IntFlag("DCHREQFLAG", 0);
  rc->set_IntFlag("PC1REQFLAG", 0);
  rc->set_IntFlag("PC2REQFLAG", 0);
  rc->set_IntFlag("PC3REQFLAG", 0);
  rc->set_IntFlag("TOFREQFLAG", 0);
  // not yet operational
  rc->set_IntFlag("EMCREQFLAG", 0);
  // assume AFS is present as at RCF
  rc->set_IntFlag("AFSABSENT", 0);
  // simVertexFlag = 0 (default) means that the BBC Z0 value will be used
  // simVertexFlag = 1 means that the same simZ0Vertex centroid value is used for all events
  // simVertexFlag = 2 means that the Z0 centroid is taken from the PISA event header for each event
  // The centroid values are modified by the Width values which are Gaussian sigma values
  Int_t simVertexFlag=2;
  Float_t simZ0Vertex=0.0, simT0Vertex=0.0;
  Float_t simZ0VertexWidth=2.0, simT0VertexWidth=0.05;
  
  rc->set_IntFlag("SIMVERTEXFLAG",simVertexFlag);
  rc->set_FloatFlag("SIMZ0VERTEX",simZ0Vertex);
  // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",simZ0VertexWidth);
  // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",simT0Vertex);
  // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",simT0VertexWidth);
  // checked in BbcSimreco only when simVertexFlag = 1 or 2

  rc->set_IntFlag("SVXACTIVE",1);
  //--------------- end 

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  //se->Verbosity(2);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  // run header and trigger setting
  se->registerSubsystem(new HeadSimreco());
  se->registerSubsystem(new TrigSimreco());

  // event counter
  if( true ) se->registerSubsystem(new MuonCounter());

  // BBC simReco
  se->registerSubsystem(new BbcSimreco("BBC"));

  // pisa is used as an input vertex.
  // it overwrites the contents of the BBC out node.
  VtxSimreco* vtx_sim = new VtxSimreco();
  vtx_sim->UseVtx(VtxSimreco::PISA);
  vtx_sim->SmearZ(false); // default is true
  vtx_sim->UseXY(true);   // default is false
  vtx_sim->OverwriteBBC(true);  // this is the default
  vtx_sim->ZVertexSigma(0.5);   // default error on the simulated vertex
  // Uncomment the lines below to use smeared "SIM" vertex in SvxCentralTrackReco 
  //vtx_sim->UseVtx(VtxSimreco::PISA);
  //vtx_sim->UseXY(true);   
  //vtx_sim->SmearX(true); 
  //vtx_sim->SmearY(true); 
  //vtx_sim->SmearZ(true); 
  //vtx_sim->XVertexSigma(0.0030);   
  //vtx_sim->YVertexSigma(0.0030);   
  //vtx_sim->ZVertexSigma(0.0030);   
  se->registerSubsystem(vtx_sim);

  // t0
  T0Simreco* t0_sim = new T0Simreco();
  t0_sim->T0Sigma(0.04);
  se->registerSubsystem(t0_sim);

  // pad chambers
  se->registerSubsystem(new PadSimreco("PAD"));

  // chiu Pad Vertexing Code, maybe needed for multiple vertices
  se->registerSubsystem(new PadVtxReco("PADVTX"));

  // The VtxReco works unchanged for both real and simulation events
  se->registerSubsystem(new VtxReco("VTX"));

  // The T0Reco works unchanged for both real and simulation events
  se->registerSubsystem(new T0Reco());

  // As of January 2, 2004 the Dch has uninitialized variable warnings from Valgrind
  // There are also log file output warning messages
  se->registerSubsystem( new DchSimreco("DCH") );

  // Time of flight detector
  se->registerSubsystem(new TofSimreco("TOF"));

  // Tof west
  se->registerSubsystem(new TfwSimreco("TFW"));

  // RICH
  se->registerSubsystem(new CrkSimreco("CRK"));

  // Aerogel subsystem as per e-mail from Narumi Kurihara on May 13, 2005
  se->registerSubsystem(new AccSimreco("ACC"));
  se->registerSubsystem(new AccReco());

  // EMCal uses the real data class
  rc->set_FloatFlag("EMCTOWERLOWGAIN", 0.0015625);
  rc->set_FloatFlag("EMCTOWERHIGHGAIN", 0.0125);
  se->registerSubsystem( new EmcReco3() );

  ///
  /// SVX modules
  ///
  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  svxpar->set_OffsetVtxToCnt(0.,0.,0.);
  svxpar->set_OffsetEastToWest(0.,0.,0.);
  svxpar->set_BeamCenter(0.,0.);
  /// setting for geometry file
  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_GeometryFileName("svxPISA.par.ideal");
  svxpar->set_UseStripThresholdDatbase(false);
  svxpar->Load_ThresholdFile("threshold_ideal.h");
  /// setting for masking of hot/dead channels
  /// If you want to set hot/dead channels from files, please remove
  /// the following comment out.
  /*  
      svxpar->set_ReadPixelMapFromFile(1);
      svxpar->set_ReadStripHotDeadFromFile(1);
      svxpar->set_PixelHotDeadChipFileName("pixelhotdeadchipmap.txt");
      svxpar->set_PixelHotDeadPixelFileName("pixelhotdeadpixelmap.txt");
      svxpar->set_StripHotDeadHybridsFileName("BadStripHybrids_347129.txt");
      svxpar->set_StripHotDeadFileName("striphotdeadmap.txt");
  */
  se->registerSubsystem(svxpar);

  SvxSimulator *svxsim = new SvxSimulator();
  svxsim->Verbosity(0);
  se->registerSubsystem(svxsim);

// Uncomment the lines below to merge raw hits from a text file
//  SvxMergeRawHits* svxmerge = new SvxMergeRawHits();
//  svxmerge->set_mergefilename("/phenix/subsys/vtx/singles/svxhits/svxhits_347141_MB.dat");
//  svxmerge->Verbosity(1);
//  se->registerSubsystem(svxmerge);

  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  svxhotdead->Verbosity(0);
  se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  svxrec->set_ThisIsSimulation();
  se->registerSubsystem(svxrec);

  SvxPriVertexSeedFinder *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  svxvtxseedfinder->Verbosity(0);
  se->registerSubsystem(svxvtxseedfinder);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);  /// 2 is default
  se->registerSubsystem(svxstandalone);

  SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

  //---------------
  // The CglReco works unchanged for both real and simulation events
  CglReco *cgl = new CglReco("CGL");
  cgl->set_SvxUseAsciiFile(true);
  se->registerSubsystem(cgl);

  //Aerogel cluster  (Needs to be after cglRec)
  se->registerSubsystem(new AccclusterReco());

  //  This is the class which makes the RICH Ring data structure
  se->registerSubsystem( new RingReco() );

  // This is the class which makes the Central Tracks nanoDST output
  // 22 corresponds to the version used in pro.78 for Run7 Au+Au
  //se->registerSubsystem(new CentraltrackReco( 24 ));

  //  This is the class which makes the GlobalEvent data on the nanoDST output
  se->registerSubsystem(new GlobalReco());
  se->registerSubsystem(new GlobalReco_central());

  // This is the class which checks for charged particles going into EMCal
  se->registerSubsystem(new ChargedvetoReco());

  //added the DC based global evaluation module
  se->registerSubsystem( new McEvalSimreco() );

  // muon arm reconstruction
  if( do_muon_arms ) {
    // unfortunately the muon arm need the sign of the magnetic field
    // and does not have yet the logic to retrieve it from the pisa event header
    // it is hard-coded here
    mMfmMT::setMapFileScale( 1.0 );
    MuonUtil::set_check_mapfile_scale( false );

    // pisa unpacking
    // The muon reconstruction is not performed
    // and only the simulated objects are created.
    // the reconstruction itself is performed in the Fun4All_RecoDST_sim afterburner macro
    se->registerSubsystem(new MuonUnpackPisa());
  }


  //=========================================
  // These fill the compactCNT storage nodes
  //=========================================
  SubsysReco *fillprojections = new FillTrackProjections();
  SubsysReco *filllineprojections = new FillTrackLineProjections();
  SubsysReco *fillpl = new FillTrackPathLengths();
  SubsysReco *filltrkhits = new FillTrackHits();
  SubsysReco *fillpadhits = new FillPadHits();
  SubsysReco *filldchits = new FillDchHits();
  SubsysReco *filltofehits = new FillTofeHits();
  SubsysReco *filltofwhits = new FillTofwHits();
  SubsysReco *fillcrkhits = new FillCrkHits();
  SubsysReco *fillacchits = new FillAccHits();
  SubsysReco *fillemchits = new FillEmcHits();

  // svx
  SubsysReco *fillsvxhits = new FillSvxHits();
  //fillsvxhits->Verbosity(1);

  se->registerSubsystem(fillprojections);
  se->registerSubsystem(filllineprojections);
  se->registerSubsystem(fillpl);
  se->registerSubsystem(filltrkhits);
  se->registerSubsystem(filldchits);
  se->registerSubsystem(fillpadhits);
  se->registerSubsystem(filltofehits);
  se->registerSubsystem(filltofwhits);
  se->registerSubsystem(fillcrkhits);
  se->registerSubsystem(fillacchits);

  // This one requires that EmcClusterContainer is already on the node tree
  se->registerSubsystem(fillemchits);

  // svx // remove due to this delete some segment entries
  //se->registerSubsystem(fillsvxhits);

  //==============================================
  // These modules read the compactCNT nodes
  // and create hits objects for each subsystem
  //================================================

  se->registerSubsystem(new RecoverTrackProjections());
  se->registerSubsystem(new RecoverTrackLineProjections());
  se->registerSubsystem(new RecoverTrackPathLengths());
  se->registerSubsystem(new RecoverTrackHits());
  se->registerSubsystem(new RecoverDchHits());
  se->registerSubsystem(new RecoverPadHits());
  se->registerSubsystem(new RecoverTofeHits());
  se->registerSubsystem(new RecoverTofwHits());
  se->registerSubsystem(new RecoverCrkHits());
  se->registerSubsystem(new RecoverAccHits());
  se->registerSubsystem(new RecoverEmcHits());
  //svx
  SubsysReco* recoverSvx = new RecoverSvxHits();
// Uncomment this line to use smeared "SIM" evrtex 
//  svxcnttrackreco->setVertexFlag(1); // 0 is default (BBC), 1 = (SIM) simulation vertex
  //recoverSvx->Verbosity(1);
  se->registerSubsystem(recoverSvx);

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
  se->registerSubsystem(new FillCNT_TofeHits());
  se->registerSubsystem(new FillCNT_TofwHits());
  se->registerSubsystem(new FillCNT_PadHits());
  se->registerSubsystem(new FillCNT_CrkHits());
  se->registerSubsystem(new FillCNT_AccHits());
  // This one needs EmcClusterContainer also
  se->registerSubsystem(new FillCNT_EmcHits());

  SubsysReco* fillCNT_SVX = new FillCNT_SvxHits();
  //fillCNT_SVX->Verbosity(1);
  se->registerSubsystem(fillCNT_SVX);

  // SvxCentralTrack
  SvxCentralTrackReco *svxcnttrackreco = new SvxCentralTrackReco();
  svxcnttrackreco->Verbosity(0);
  se->registerSubsystem( svxcnttrackreco );

  // dumper
  if( false ) {
    gSystem->Load( "libphnodedump" );
    se->registerSubsystem( new Dumper() );
  }

  ///////////////////////////////////////////
  // InputManager
  ///////////////////////////////////////////
  Fun4AllInputManager *input_manager = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(input_manager);

  ///////////////////////////////////////////
  // OutputManagers Set up functions
  ///////////////////////////////////////////
  if( dstout ) {
    Fun4AllDstOutputManager *manager  = new Fun4AllDstOutputManager("SIMDST", dstout);
    
    // run and event header
    manager->AddNode("RunHeader");
    manager->AddNode("EventHeader");
    manager->AddNode("TrigLvl1");
    // pisa nodes
    manager->AddNode("fkin");
    manager->AddNode("pythia");
    manager->AddNode("primary");
    manager->AddNode("header");
    // global vertex and t0
    manager->AddNode("T0Out");
    manager->AddNode("VtxOut");
    // BBC
    manager->AddNode("bbcghit");
    manager->AddNode("BbcOut");
    manager->AddNode("BbcRaw");
    // ZDC
    manager->AddNode("ZdcOut");
    manager->AddNode("ZdcRaw");
    // DCH
    manager->AddNode("dcghit");
    manager->AddNode("DchHitLineTablev1");
    manager->AddNode("DchHitLineTable");
    manager->AddNode("DchTrack");
    manager->AddNode("dDchTracksPerf");
    manager->AddNode("dDchTracksExtPerf");
    manager->AddNode("dDchGhitHits");
    // PC
    manager->AddNode("pc1ghit");
    manager->AddNode("pc2ghit");
    manager->AddNode("pc3ghit");
    manager->AddNode("dPc1GhitClus");
    manager->AddNode("dPc2GhitClus");
    manager->AddNode("dPc3GhitClus");
    manager->AddNode("Pc1Cluster");
    manager->AddNode("Pc2Cluster");
    manager->AddNode("Pc3Cluster");
    manager->AddNode("Pc1Raw");
    manager->AddNode("Pc2Raw");
    manager->AddNode("Pc3Raw");
    // RICH
    manager->AddNode("crkghit");
    manager->AddNode("CrkHit");
    // these nodes, when added to the output
    // makes the read-back of the DSTs crash
    // it is removed for the moment
    // they can be added back starting from pro.80
    // manager->AddNode("CrkRing");
    // manager->AddNode("CrkRingBack");
    // TOF
    manager->AddNode("tofghit");
    manager->AddNode("TofOut");
    manager->AddNode("dTofGdigi");
    manager->AddNode("dTofGhitGdigi");
    manager->AddNode("dTofGdigiRec");
    // TOF West
    manager->AddNode("TofwRaw");
    manager->AddNode("TofwHit");
    // Aerogel
    manager->AddNode("AerGeaHits");
    manager->AddNode("AccCluster");
    manager->AddNode("AccRaw");
    manager->AddNode("AccHit");
    // EMCal
    manager->AddNode("emcghit");
    manager->AddNode("emcClusterContainer");
    manager->AddNode("emcTowerContainer");
    // additional EMCal evaluation nodes
    if( rc->FlagExist("EVALUATIONFLAG") 
	&& rc->get_IntFlag("EVALUATIONFLAG")==1 ) {
      // Evaluation output from EMCal
      manager->AddNode("dEmcGeaClusterTrack");
      manager->AddNode("dEmcGeaTrack");
      manager->AddNode("dEmcGeaTrackCluster");
    }
    // SVX
    manager->AddNode("SvxPisaHit");
    manager->AddNode("SvxGhitList");
    manager->AddNode("SvxRawhitList");
    manager->AddNode("SvxGhitRawhitList");
    manager->AddNode("SvxClusterList");
    manager->AddNode("SvxRawhitClusterList");
    manager->AddNode("SvxGhitClusterList");
    manager->AddNode("SvxSegmentList");
    manager->AddNode("SvxCentralTrackList");
    // CGL
    manager->AddNode("CglTrack");
    manager->AddNode("CglTrackBack");
    manager->AddNode("PHDchTrackOut");
    manager->AddNode("PHTrackOut");
    manager->AddNode("PHTrackOutBack");
    // copied from CNT node
    manager->AddNode("PHCentralTrack");
    manager->AddNode("PHGlobal");
    manager->AddNode("PHGlobal_CENTRAL");
    manager->AddNode("PHGlobal_MUON");
    // simulation track evaluation
    manager->AddNode("McSingle");

    se->registerOutputManager(manager);
  }

  ///////////////////////////////////////////
  // open input file
  se->fileopen(input_manager->Name(),filein);

  // process input events
  gBenchmark->Start("eventLoop");
  se->run(nEvents);
  se->End();
  gBenchmark->Show("eventLoop");

  // If you do not see this message, the job failed
  cout << "Completed reconstruction." << endl;
}
