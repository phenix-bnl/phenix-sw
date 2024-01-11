void run_embed(
  char *mcDST="/phenix/subsys/vtx/lebedev/embedding/simdst/simDST_electrons_000.root", 
  char *realDST="/phenix/subsys/vtx/lebedev/embedding/reco/DST_Embed_MB-0000347128-0001.root",
  string outfile="test.root", 
  int nevt=1000)

///
/// mcDST   : DST of simulation
/// realDST : DST of real data
/// outfile : output file name
/// nevt    : number of event (0=all)
///
///
/// mcDST must have following nodes
/// - SvxGhitList
/// - VtxOut
/// - McSingle (McEvalSingleList)
/// - PHCentralTrack
///
/// realDST mus have following nodes
/// - SvxRawhitList
/// - VtxOut
/// - RunHeader
/// - EventHeader
/// - PreviouseEvent
/// - TrigLvl1
/// - TrigRunLvl1
/// - BbcOut
/// - PHGlobal
///
/// Please check detector geometry file.
/// Currently, svxPISA.par is set, but you must use the geometry file
/// you used in simulation.
///
{

int dataFlag = 0;

//  gSystem->Load("/phenix/subsys/vtx/lebedev/embedding/embed/install/lib/libsvx.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libPHPythia.so");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs");
//  gSystem->Load("libcompactCNT.so");
  gSystem->Load("/phenix/subsys/vtx/lebedev/embedding/install/lib/libembedreco.so");
  gSystem->Load("libsimreco");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
  recoConsts *rc = recoConsts::instance();
  // MC TopNode name
  rc->set_CharFlag("EMBED_MC_TOPNODE","SINGLE");
  // real event TopNode name
  rc->set_CharFlag("EMBED_REAL_TOPNODE","REAL");

  // 2 means PISA-To-DST
  rc->set_IntFlag("SIMULATIONFLAG",2);

  int run_number = 347128;
  rc->set_IntFlag("RUNNUMBER",run_number);

  rc->set_IntFlag("DCHREQFLAG", 0);
  rc->set_IntFlag("PC1REQFLAG", 0);
  rc->set_IntFlag("PC2REQFLAG", 0);
  rc->set_IntFlag("PC3REQFLAG", 0);
  rc->set_IntFlag("TOFREQFLAG", 0);

  rc->set_IntFlag("AFSABSENT", 0);


  Int_t simVertexFlag=2;
  Float_t simZ0Vertex=0.0, simT0Vertex=0.0;
  Float_t simZ0VertexWidth=2.0, simT0VertexWidth=0.05;

  rc->set_IntFlag("SIMVERTEXFLAG",simVertexFlag);
  rc->set_FloatFlag("SIMZ0VERTEX",simZ0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",simZ0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",simT0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",simT0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2

/*
  SubsysReco *head = new HeadSimreco();
  head->Verbosity(0);
  se->registerSubsystem(head );
  se->registerSubsystem( new TrigSimreco() );
*/
//  if( true ) se->registerSubsystem( new MuonCounter() );

//  se->registerSubsystem(new BbcSimreco("BBC"));
/*
  VtxSimreco* vtx_sim = new VtxSimreco();
  vtx_sim->UseVtx( VtxSimreco::PISA );
  vtx_sim->SmearZ( false ); // default is true
  vtx_sim->UseXY( true );   // default is false
  vtx_sim->OverwriteBBC( true );  // this is the default
  vtx_sim->ZVertexSigma( 0.5 );   // default error on the simulated vertex
  se->registerSubsystem( vtx_sim );

  T0Simreco* t0_sim = new T0Simreco();
  t0_sim->T0Sigma(0.04);
  se->registerSubsystem( t0_sim );
*/
//  se->registerSubsystem(new VtxReco("VTX"));

//  se->registerSubsystem(new T0Reco());



/*
  VtxSimreco* vtx_sim = new VtxSimreco();
  vtx_sim->UseVtx( VtxSimreco::PISA );
  vtx_sim->UseXY( true );   // default is false
  vtx_sim->OverwriteBBC( true );  // this is the default
  vtx_sim->SmearX( true ); // default is true
  vtx_sim->SmearY( true ); // default is true
  vtx_sim->SmearZ( true ); // default is true
  vtx_sim->XVertexSigma( 0.01 );
  vtx_sim->YVertexSigma( 0.01 );
  vtx_sim->ZVertexSigma( 0.01 );
  se->registerSubsystem( vtx_sim );
*/


  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_GeometryFileName("svxPISA.par");
  svxpar->set_OffsetVtxToCnt(0.0, 0.0, 0.0);
  svxpar->set_OffsetEastToWest(0.0, 0.0, 0.0);
  svxpar->set_BeamCenter(0.0, 0.0);
//  svxpar->Load_ThresholdFile("svx_threshold.dat");
//  svxpar->set_UseStripThresholdDatbase(false);
  //svxpar->Verbosity(1);
  se->registerSubsystem(svxpar);

  SvxEmbedSimhit *svxembed = new SvxEmbedSimhit();
  svxembed->set_StripixelNoise(0.0); // no noise
  svxembed->Verbosity(0);
  se->registerSubsystem(svxembed);

  SvxApplyHotDead *svxapplyhotdead  = new SvxApplyHotDead();
  //svxapplyhotdead->Verbosity(1);
  se->registerSubsystem(svxapplyhotdead);

  SvxReco *svxreco = new SvxReco();
  svxreco->Verbosity(0);
  svxreco->set_ThisIsSimulation();
  svxreco->Load_ThresholdFile("threshold_zero.h");
  svxreco->set_UseStripThresholdDatbase(false);
  svxreco->set_StripixelAdcSumThreshold(0);
  se->registerSubsystem(svxreco);

  SubsysReco *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  se->registerSubsystem(svxvtxseedfinder);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  if(dataFlag==2) svxstandalone->setPPFlag(true);
  svxstandalone->setVertexRecoFlag(2);
  //svxstandalone->setVertexRecoFlag(0); /// ???? force to use beam center
  se->registerSubsystem( svxstandalone );

  SubsysReco *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

  SvxCentralTrackReco *svxcnttrackreco = new SvxCentralTrackReco();
  //svxcnttrackreco->Verbosity(4);
  svxcnttrackreco->setSearchWindowFlag(1); // default(0), for single particle sim, use(1) = tigher cut
  svxcnttrackreco->setVertexFlag(1);   // use SIM vertex
  se->registerSubsystem( svxcnttrackreco );

//  SvxSelectClusters *svxselect = new SvxSelectClusters();
//  svxselect->Verbosity(0);
//  se->registerSubsystem(svxselect);

/*
  // svx compactCNTs
  FillSvxHits *fillsvxhits = new FillSvxHits();
  //fillsvxhits->Verbosity(1);
  fillsvxhits->setSaveOnlySelectedHits(true);
  se->registerSubsystem(fillsvxhits);
*/

  Fun4AllDstOutputManager *manager = new Fun4AllDstOutputManager("DST_SVX",outfile); 
  manager->AddNode("RunHeader");
  manager->AddNode("EventHeader");
  manager->AddNode("PreviousEvent");
  manager->AddNode("TrigLvl1");
  manager->AddNode("TrigRunLvl1");
  manager->AddNode("VtxOut");
  manager->AddNode("McSingle");
  //  manager->AddNode("SvxGhitList");
  //  manager->AddNode("SvxRawhitList");
  manager->AddNode("SvxClusterList");
  //manager->AddNode("SvxSelectedClusterList");
  //manager->AddNode("SvxSegmentList");
  manager->AddNode("SvxCentralTrackList");
  manager->AddNode("PHCentralTrack");
  manager->AddNode("PHGlobal");
  //manager->AddNode("SvxHit_VarArray");
  //manager->AddNode("SvxTrack_VarArray");
  //manager->AddNode("SvxCentralTrack_VarArray");
  se->registerOutputManager(manager);
 
  Fun4AllInputManager *in1 = new Fun4AllNoSyncDstInputManager("DSTin1","DST","SINGLE"); 
  Fun4AllInputManager *in2 = new Fun4AllDstInputManager("DSTin2","DST","REAL");
  //in1->AddListFile(mclist);   //read into "SINGLE" Node  
  //in2->AddListFile(reallist); //read into "REAL" Node
  in1->AddFile(mcDST);
  in2->AddFile(realDST);
  se->registerInputManager(in1);
  se->registerInputManager(in2);

  cout << "Analysis started " << endl;
  gSystem->Exec("date");

  cout <<endl<<endl;
  cout <<"DST FILE is opened"<<endl;
  cout <<endl<<endl;
  
  se->run(nevt);     //process all events in the run
  if ( nevt!=0 ) { se->fileclose("DSTin1"); }
  // this is necessary only if you don't process all events

  se->End();
  
  cout << "Analysis finished " << endl;
  gSystem->Exec("date");
}

