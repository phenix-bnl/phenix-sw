/*! 
  code to unpack PRDF and create a DST feasible for embedding. Cesar
*/

#include <string>
#include <sstream>
#include <iostream>

void Fun4FVTX_UnpackPRDF(int nEvents = 100,
			  char *inputfile = "EVENTDATA_P00-0000375910-0080.PRDFF",
			  char *dstfile = "dst_out.root",
			  char *event_file = "event.txt")

{  
  // flags
  bool use_min_bias = true;
  bool use_2d_trigger = false;
  bool write_dst = true;
  bool do_hack = false;
  bool use_bbc_z_cut = true;
  bool do_clustering_tracking = true;
  bool make_pdst = false;

  string FileName = inputfile;
  int length = FileName.size();
  string FileTag = FileName.substr(length-22,16);
  
  istringstream RUNNUMBER(FileTag.substr(1,10));
  int runnumber;
  RUNNUMBER >> runnumber;
  istringstream SEGNUMBER(FileTag.substr(12,4));
  int segnumber;
  SEGNUMBER >> segnumber;

  string qa_file = Form("qafile_updatedcuts-%d-%d.root",runnumber,segnumber);

  gSystem->Exec(Form("/afs/rhic/phenix/software/calibration/data/LuxorLinker.pl -1 %d",runnumber));
  cout << "Fun4Muons_UnpackPRDF - inputfile=" << inputfile << endl;
  if( dstfile ) cout << "Fun4Muons_UnpackPRDF - dstfile=" << dstfile << endl;
  if( event_file ) cout << "Fun4Muons_UnpackPRDF - event_file=" << event_file << endl;

  // load libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco" );
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");
  gSystem->Load("libfvtx_subsysreco.so");
  gSystem->Load("librxnp_subsysreco");


  if ( gSystem->Load("librecal.so") )
    gSystem->Exit(66);

  cerr << "libraries loaded.\n";
  
  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", 1);

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);
  TMD5 *md5 = new TMD5();
  TMD5 *m5tmp = md5->FileChecksum("pisafile.dat.cZ");
  rc->set_CharFlag("md5_pisafile.dat.cZ",m5tmp->AsString());
  delete m5tmp;
  m5tmp = md5->FileChecksum("fieldIntegral.dat");
  rc->set_CharFlag("md5_fieldIntegral.dat",m5tmp->AsString());
  delete m5tmp;
  delete md5;

  //  rc->set_CharFlag("TRIGSETUPFILE","Run10AuAuTrigSetupMuon");
  rc->Print();

  // Print database readouts
  TMutDatabaseCntrl::set_database_access("use_local_dead_channel_file",false);
  TMutDatabaseCntrl::print();

  //  TMutExtVtx::get().set_vtx_source( TMutExtVtx::BBC );
  
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////
  // global detectors subsystem
  se->registerSubsystem( new SyncReco() );
  se->registerSubsystem( new HeadReco() );
  se->registerSubsystem( new TrigReco() );

  MuonCounter* counter = new MuonCounter();
  se->registerSubsystem( counter );
  counter->set_event_dump(1000);

  // minimum bias filter
  if( use_min_bias )
    {
      TrigSelect *minbias = new TrigSelect("MB");
      minbias->AddTrigger("BBCLL1(>1 tubes) narrowvtx");
      minbias->AddTrigger("BBCLL1(>1 tubes) narrowvtx CopyA");
      minbias->AddTrigger("BBCLL1(>1 tubes) narrowvtx CopyB");
      minbias->AddTrigger("BBCLL1(>1 tubes) narrowvtx CopyC");
      minbias->SetReturnCode("ABORT");
      se->registerSubsystem(minbias);
      cout << "Fun4Muons_RecoMultiNDST - adding minimum bias trigger requirement" << endl;
    }
  
  if ( use_2d_trigger )
    {
      TrigSelect *muid2d = new TrigSelect("MUID2D");
      muid2d->AddTrigger("(MUIDLL1_N2D||S2D)&BBCLL1(noVtx)");
      muid2d->SetReturnCode("ABORT");
      se->registerSubsystem(muid2d);
      cout << "Fun4Muons_RecoMultiNDST - adding MUIDLL12D trigger requirement" << endl;
    }

  se->registerSubsystem( new BbcReco() );
  se->registerSubsystem( new ZdcReco() );
  se->registerSubsystem( new VtxReco() );

  // global reconstruction
  se->registerSubsystem( new GlobalReco() );  

  // bbc_z cut
  if( use_bbc_z_cut ) 
    {
      MuonTrigFilter *z_bbc_filter = new MuonTrigFilter( "BBC_FILTER", MuonTrigFilter::Z_VERTEX);
      z_bbc_filter->set_z_bbc_window( -15, 15 );
      se->registerSubsystem( z_bbc_filter );
    }

  //  se->registerSubsystem( new MpcReco() );
  //  se->registerSubsystem( new MuonReadbackDST() );
  //  se->registerSubsystem( new RxnpReco() );
  //  se->registerSubsystem( new RpSumXYReco() );
  
  //  se->registerSubsystem( new ReactionPlaneReco() );
  
  // prdf unpacker
  SubsysReco *muon_unpack = new MuonUnpackPRDF();
  muon_unpack->Verbosity(1);
  se->registerSubsystem( muon_unpack );

  MuiooReco* muioo_reco = new MuiooReco();
  muioo_reco->set_asymm_cut_par(400);
  muioo_reco->set_max_occupancy_per_arm(400);
  se->registerSubsystem( muioo_reco );

  se->registerSubsystem( new MuonDev() );

 // SVX reconstruction"

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
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

  /*
  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  // svxrec->Load_ThresholdFile("threshold.h");
  svxrec->set_UseStripThresholdDatbase(true);
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
  svxstandalone->setWindowScale(10);
  se->registerSubsystem(svxstandalone);

  SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);
  */

  FvtxUnpackPRDF *Fvtx_unpack = new FvtxUnpackPRDF();
  se->registerSubsystem(Fvtx_unpack);

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->set_use_svx_cluster(true);
  fvtxreco->set_do_mutr_matching(false); 
  se->registerSubsystem(fvtxreco);

  // primary vertex from FVTX just to get vertex for embedding
  FvtxPrimVertex* fvtxprimvtx = new FvtxPrimVertex();
  fvtxprimvtx->set_fvtx_Rres(0.1); //crossing window (0.1 for CuAu, 0.5 for pp)
  fvtxprimvtx->set_source( FvtxPrimVertex::Tracks ); // or FvtxPrimVertex::Coordinate
  se->registerSubsystem(fvtxprimvtx);

  // recalibrator (needed for centrality)
  MasterRecalibratorManager * recal = new MasterRecalibratorManager();
  recal->Verbosity(10);
  se->registerSubsystem( recal );

  // create a table with event-by-event vertex/centrality to be used in PHPythia
  MuonAnaTuples* ana_tuple = new MuonAnaTuples( "MUONANATUPLES" );
  se->registerSubsystem( ana_tuple );
  ana_tuple->set_event_filename( event_file );
  ana_tuple->set_vertex_name( "FVTX" );
  ana_tuple->set_flags( MuonAnaTuples::VERTEX | MuonAnaTuples::CENTRALITY | MuonAnaTuples::REACTION_PLANE);
  //  ana_tuple->set_flags( MuonAnaTuples::VERTEX | MuonAnaTuples::CENTRALITY);

  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////

  // dst
  if( write_dst && dstfile ) 
    {
      Fun4AllDstOutputManager *dstManager= new Fun4AllDstOutputManager("DSTOUT",dstfile);
        
      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");
      dstManager->AddNode("VtxOut");
      dstManager->AddNode("BbcOut");
      dstManager->AddNode("ZdcOut");
      dstManager->AddNode("BbcRaw");
      dstManager->AddNode("ZdcRaw");
      dstManager->AddNode("TrigLvl1");
      dstManager->AddNode("PHGlobal");
      dstManager->AddNode("RpSumXYObject");
  
      // Muioo nodes
      dstManager->AddNode("TMuiHitO");

      // Mutoo nodes
      dstManager->AddNode("TMutHit");
      //      dstManager->AddNode("TMutClus");
      //      dstManager->AddNode("TMutCoord");
      //      dstManager->AddNode("TMutGapCoord");

      // FVTX nodes
      dstManager->AddNode("TFvtxHit");
      //      dstManager->AddNode("TFvtxClus");
      //      dstManager->AddNode("TFvtxCoord");
      //      dstManager->AddNode("TFvtxTrk");

      // VTX nodes
      dstManager->AddNode("SvxRawhitList");

      se->registerOutputManager(dstManager);
    }
  
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(inputfile);
  //  in->AddListFile(input_file);
  se->registerInputManager(in);

  se->run(nEvents);

  se->End();

  cout << "Completed reconstruction." << endl;
}
