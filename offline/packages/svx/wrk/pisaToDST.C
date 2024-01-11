// $Id: pisaToDST.C,v 1.5 2013/05/02 18:35:04 hachiya Exp $
/*!
   \file pisaToDST.C
   \brief pisa to DST reconstruction chain
   \author <a href="mailto:pereira@hep.saclay.cea.fr">Hugo Pereira</a>
   \version $Revision: 1.5 $
   \date $Date: 2013/05/02 18:35:04 $
*/
//  int run_number = 349369

// dataFlag : 
//    0=Single : SimVtx in VtxOut is automatically used (since only SimVtx is kept in the VtxOut)
//    1=Hijing : Precise Vtx ordered by VtxOut is used as same as real data.
//               You need to set the beam offset (svxpar->set_BeamCenter(x, y)) by hand. default is (0,0)
//    2=Pythia : ppflag=1 in SvxStandaloneReco

// runnumber used for hot dead map
//  run346033 : run11, normal  field before 4th pixel repair
//  run347128 : run11, reverse field after 4th pixel repair

// B-field
//  Direction of magnetic field in simulation reconstruction is taken 
//  from the PISA input file, without using run number.

void pisaToDST(
  Int_t nEvents = 100,
  char *filein="PISAEvent.root",
  char *dstout = "simDST.root",
//  char *cntqaout = "SvxCntQA.root",
  int run_number = 347128,
  int dataFlag = 0
)
{

  // print output
  cout << "pisaToDST - nEvents: " << nEvents << endl;
  cout << "pisaToDST - filein: " << filein << endl;
  if( dstout ) cout << "pisaToDST - dstout: " << dstout << endl;
  cout << "pisaToDST - run_number: " << run_number << endl;
  cout << endl;

  // allow to disable muon simulations
  // they are enabled by default
  bool do_muon_arms = false;

  // load libraries
  gSystem->Load("libsvx");
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs");
  gSystem->Load("libsimreco");
  gSystem->Load("libcompactCNT.so");
  gSystem->Load("libSvxDstQA.so");

  gROOT->ProcessLine(".L pisaToDST_IOManager.C");

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
  Flags to abort event if required number of GEANT hits is not present in the subsystem
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

  //--------------- added
  // Kalman Flags
  rc->set_FloatFlag("KALPMIN",0.400);
  rc->set_IntFlag("KALFILTERDCUV",1);
  rc->set_IntFlag("KALFIT",1);
  rc->set_IntFlag("KALTESTNTUPLE",0);
  rc->set_IntFlag("KALREGENDERIV",1);
  rc->set_IntFlag("KALUSEDCHX1X2",1);
  rc->set_IntFlag("KALUSEDCHUV",1);
  rc->set_IntFlag("KALUSEPC1",1);
  rc->set_IntFlag("KALUSEPC2",0);
  rc->set_IntFlag("KALUSEPC3",0);
  rc->set_IntFlag("KALUSETEC",0);
  rc->set_IntFlag("KALUSETOF",0);
  rc->set_IntFlag("KALUSEEMC",0);
  rc->set_IntFlag("KALUSESVX",1);
  rc->set_IntFlag("KALSVXASSOC",0); // set to 0 if you want to use cgl results for svx
 
  // simVertexFlag = 0 (default) means that the BBC Z0 value will be used
  // simVertexFlag = 1 means that the same simZ0Vertex centroid value is used for all events
  // simVertexFlag = 2 means that the Z0 centroid is taken from the PISA event header for each event
  // The centroid values are modified by the Width values which are Gaussian sigma values
  Int_t simVertexFlag=2;
  Float_t simZ0Vertex=0.0, simT0Vertex=0.0;
  Float_t simZ0VertexWidth=2.0, simT0VertexWidth=0.05;
  
  rc->set_IntFlag("SIMVERTEXFLAG",simVertexFlag);
  rc->set_FloatFlag("SIMZ0VERTEX",simZ0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",simZ0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",simT0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",simT0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2

  rc->set_IntFlag("SVXACTIVE",0);
  //--------------- end 


  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  // run header and trigger setting
  SubsysReco *head = new HeadSimreco();
  head->Verbosity(2);
  se->registerSubsystem(head );
  se->registerSubsystem( new TrigSimreco() );

  // event counter
  if( true ) se->registerSubsystem( new MuonCounter() );

  // BBC simReco
  se->registerSubsystem(new BbcSimreco("BBC"));

  // pisa is used as an input vertex.
  // it overwrites the contents of the BBC out node.
  VtxSimreco* vtx_sim = new VtxSimreco();
  vtx_sim->UseVtx( VtxSimreco::PISA );
  vtx_sim->SmearZ( false ); // default is true
  vtx_sim->UseXY( true );   // default is false
  vtx_sim->OverwriteBBC( true );  // this is the default
  vtx_sim->ZVertexSigma( 0.5 );   // default error on the simulated vertex
  se->registerSubsystem( vtx_sim );

  // t0
  T0Simreco* t0_sim = new T0Simreco();
  t0_sim->T0Sigma(0.04);
  se->registerSubsystem( t0_sim );

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

  // Time expansion chamber
  //se->registerSubsystem( new TecSimreco("TEC"));

  // Time of flight detector
  se->registerSubsystem(new TofSimreco("TOF"));

  // Tof west
  se->registerSubsystem(new TfwSimreco("TFW"));

  //// HBD
  //se->registerSubsystem(new HbdSimreco("HBD"));

  // RICH
  se->registerSubsystem(new CrkSimreco("CRK"));

  // Aerogel subsystem as per e-mail from Narumi Kurihara on May 13, 2005
  se->registerSubsystem(new AccSimreco("ACC"));
  se->registerSubsystem(new AccReco());

  // EMCal uses the real data class
  rc->set_FloatFlag("EMCTOWERLOWGAIN", 0.0015625);
  rc->set_FloatFlag("EMCTOWERHIGHGAIN", 0.0125);
  se->registerSubsystem( new EmcReco3() );

  //------------------------------
  // VTX simulation
  // register first 
  SvxParManager *svxpar = new SvxParManager();
  (dynamic_cast<SvxParManager*>svxpar)->set_ReadGeoParFromFile(1);  // read parameters from ascii file
  (dynamic_cast<SvxParManager*>svxpar)->set_OffsetVtxToCnt  (0.0, 0.0, 0.0);
  (dynamic_cast<SvxParManager*>svxpar)->set_OffsetEastToWest(0.0, 0.0, 0.0);
  (dynamic_cast<SvxParManager*>svxpar)->set_BeamCenter(0.0, 0.0);
//  (dynamic_cast<SvxParManager*>svxpar)->set_BeamCenter(0.1, 0.1);
  svxpar->Load_ThresholdFile("threshold_zero.h");
  svxpar->set_UseStripThresholdDatbase(false);
  se->registerSubsystem(svxpar);



  SubsysReco *svxsim     = new SvxSimulator();
  se->registerSubsystem(svxsim);


  SubsysReco *svxapplyhotdead  = new SvxApplyHotDead();
//  svxapplyhotdead->Verbosity(1);
//  se->registerSubsystem(svxapplyhotdead);
  


  SvxReco *svxreco     = new SvxReco();
//  svxreco->Verbosity(0);
  svxreco->set_ThisIsSimulation();
  //svxreco->set_StripixelAdcThreshold(0);
  svxreco->set_StripixelAdcSumThreshold(0);
  se->registerSubsystem(svxreco);

  SubsysReco *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  se->registerSubsystem(svxvtxseedfinder);
  
  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  if(dataFlag==2) svxstandalone->setPPFlag(true);
  svxstandalone->setVertexRecoFlag(2);
  se->registerSubsystem( svxstandalone );

  SubsysReco *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(3);
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
  if( do_muon_arms )
  {

    // unfortunately the muon arm need the sign of the magnetic field
    // and does not have yet the logic to retrieve it from the pisa event header
    // it is hard-coded here
    mMfmMT::setMapFileScale( 1.0 );
    MuonUtil::set_check_mapfile_scale( false );

    // pisa unpacking
    // The muon reconstruction is not performed
    // and only the simulated objects are created.
    // the reconstruction itself is performed in the Fun4All_RecoDST_sim afterburner macro
    se->registerSubsystem( new MuonUnpackPisa() );

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
  //SubsysReco *fillsvxhits = new FillSvxHits();
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
  //SubsysReco* recoverSvx = new RecoverSvxHits();
  //recoverSvx->Verbosity(1);
  //se->registerSubsystem(recoverSvx);

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

  //SubsysReco* fillCNT_SVX = new FillCNT_SvxHits();
  //fillCNT_SVX->Verbosity(1);
  //se->registerSubsystem(fillCNT_SVX);


  // SvxCentralTrack
  SvxCentralTrackReco *svxcnttrackreco = new SvxCentralTrackReco();
//  svxcnttrackreco->Verbosity(5);
  se->registerSubsystem( svxcnttrackreco );

  SvxCentralTrackReco* svxcentraltrackbg = new SvxCentralTrackReco("SVXCENTRALTRACKRECOBACK");
  svxcentraltrackbg->RndmAssocDchFlag(1);
//  svxcentraltrack->setClearOtherLinkFlag(false);
//  svxcentraltrack->Verbosity(4);
  se->registerSubsystem(svxcentraltrackbg);


  SvxSelectClusters* svxselect = new SvxSelectClusters();
  svxselect->Verbosity(0);
  se->registerSubsystem(svxselect);

  // svx compactCNTs
  FillSvxHits *fillsvxhits = new FillSvxHits();
  fillsvxhits->Verbosity(0);
  //fillsvxhits->setSaveOnlySelectedHits(true);
  se->registerSubsystem(fillsvxhits);


  // CntQA
//  SubsysReco *svxcntqa  = new SvxCntQA(cntqaout);
//  (dynamic_cast<SvxCntQA*>svxcntqa)->setSimMode(1);
//  (dynamic_cast<SvxCntQA*>svxcntqa)->setGoodOnlyFlag(false);
//  se->registerSubsystem(svxcntqa);




  // dumper
  if( false )
  {
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
  if( dstout ) DST_IOManager(dstout, se);

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
