// $Id: Fun4Muons_RecoPRDF.C,v 1.47 2013/01/08 19:42:49 brooks Exp $
/*!
  prdf analysis loop for real data. Unpack the data,
  creates a unreconstructed DST
*/

void Fun4Muons_RecoPRDF(
  int nEvents = 20,
  char *inputfile = "data.prdf",
  char *dstfile   = "dst_out.root",
  char *ana_file = "muon_ana_ntuples.root",
  //int run_number = 360502
  int run_number = 365165
)

{

  // flags
  bool use_lvl2 = false;

  // load libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfvtx_subsysreco");

  cerr << "libraries loaded.\n";

  Int_t iTest;
  cout << "\n debug pause " << endl;
  cin >> iTest;
  cout << "\n iTest = " << iTest << endl;


  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", 1);

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);
  rc->set_IntFlag("RUNNUMBER", run_number);
  if (use_lvl2) {
    // Set Lvl2 flags
    rc->set_IntFlag("LVL2_REAL_DATA",1);
    rc->set_IntFlag("LVL2_YEAR",4);
    rc->set_IntFlag("FORCE_LVL2",1);
    rc->set_IntFlag("LVL2_USE_ASCII_DB",1);
    rc->set_CharFlag("LVL2_DB_DIR","/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN4_REAL");
    rc->set_CharFlag("Run2Lvl2AlgoName", "");
  }

  // mutoo vertex source configuration
  // this allows to print which vertex is used and its value
  TMutExtVtx::get().set_verbosity( MUTOO::SOME );

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  // Counter
  se->registerSubsystem( new MuonCounter() );

  // global detectors subsystem
  se->registerSubsystem( new HeadReco() );
  se->registerSubsystem( new TrigReco( ));
  se->registerSubsystem( new BbcReco() );
  se->registerSubsystem( new ZdcReco() );
  se->registerSubsystem( new VtxReco() );

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

  // muon prdf unpacker
  MuonUnpackPRDF* muon_unpack_prdf( new MuonUnpackPRDF() );
  muon_unpack_prdf->Verbosity( 1 );
  muon_unpack_prdf->set_flag(MuonUnpackPRDF::SKIP_ZERO_SUPPRESSION,1);
  se->registerSubsystem( muon_unpack_prdf );

  // mutoo reconstruction
  se->registerSubsystem( new MuiooReco() );
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

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  // svxrec->Load_ThresholdFile("threshold.h");
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
  SubsysReco *fvtx_unpack = new FvtxUnpackPRDF();
  fvtx_unpack->Verbosity(0);
  se->registerSubsystem( fvtx_unpack );

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->run_after_burner(false);         // Adds VTX hits to tracking
  fvtxreco->set_use_svx_cluster(false);     // Uses smeared PISA hits if false
  se->registerSubsystem(fvtxreco);

  //FvtxMCEval* fvtxeval = new FvtxMCEval();
  FvtxEval* fvtxeval = new FvtxEval("FvtxEval","fvtx_eval.root");
  se->registerSubsystem(fvtxeval);
  //se->registerSubsystem( new MuonAnaTuples(ana_file) );

  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////

  // dst
  if( dstfile ) {
    Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",  dstfile);

    dstManager->AddNode("RunHeader");
    dstManager->AddNode("EventHeader");
    dstManager->AddNode("VtxOut");
    dstManager->AddNode("BbcOut");
    dstManager->AddNode("BbcRaw");
    dstManager->AddNode("ZdcOut");
    dstManager->AddNode("ZdcRaw");
    dstManager->AddNode("TrigLvl1");
    dstManager->AddNode("L2Decision");
    dstManager->AddNode("Lvl2OutArray");

    // Muioo nodes
    dstManager->AddNode("TMuiHitO");

    // Mutoo nodes
    dstManager->AddNode("TMutHit");

    se->registerOutputManager(dstManager);
  }


  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(inputfile);
  se->registerInputManager(in);
  se->run(nEvents);
  se->End();

  cout << "Completed reconstruction." << endl;
}
