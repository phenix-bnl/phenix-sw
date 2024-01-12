// $Id: Fun4Muons_UnpackPRDF.C,v 1.20 2009/12/03 23:54:33 hpereira Exp $
/*!
  prdf analysis loop for real data. Unpack the data,
  creates a unreconstructed DST
*/

void Fun4Muons_UnpackPRDF(
  int nEvents = 20,
  char *inputfile = "data.prdf",
  char *dstfile   = "dst_out.root"
)

{

  // flags
  bool use_lvl2 = false;

  // load libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");

  cerr << "libraries loaded.\n";

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);
  if (use_lvl2) {
    // Set Lvl2 flags
    rc->set_IntFlag("LVL2_REAL_DATA",1);
    rc->set_IntFlag("LVL2_YEAR",4);
    rc->set_IntFlag("FORCE_LVL2",1);
    rc->set_IntFlag("LVL2_USE_ASCII_DB",1);
    rc->set_CharFlag("LVL2_DB_DIR","/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN4_REAL");
    rc->set_CharFlag("Run2Lvl2AlgoName", "");
  }

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

  // prdf unpacker
  SubsysReco *muon_unpack = new MuonUnpackPRDF();
  muon_unpack->Verbosity(1);
  se->registerSubsystem( muon_unpack );

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

  pfileopen(inputfile);
  prun(nEvents);
  se->End();

  cout << "Completed reconstruction." << endl;
}
