// $Id: Fun4Muons_ReadDST.C,v 1.28 2009/12/03 23:54:33 hpereira Exp $
/*!
  dst analysis loop for real data dst
  read nodes from the DST
  creates a NanoDST and a picoDST
*/
void Fun4Muons_ReadDST(int nEvents = 100,
  char *inputfile = "dst.root",
  char *ndstfile = "ndst_out.root",
  char *pdstfile = "pdst_out.root",
  char *qafile = "qa_out.root"
)
{

  // ndst/pdst output
  bool write_ndst = true;
  bool write_pdst = true;

  // mutoo evaluators
  bool do_evaluation = true;

  // qa
  bool do_qa = true;

  // mc flag
  bool is_mc = false;
  bool use_RPC = false;

  // libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco" );
  gSystem->Load("libfun4allfuncs_muons");

  if( do_evaluation )
  {
    // evaluation requires some extra reconstruction modules
    // from muon_subsysreco
    gSystem->Load("libmuon_subsysreco");
  }

  gSystem->Load("liblvl2");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libMWG_interface");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);

  // mutoo vertex source configuration
  // this allows to print which vertex is used and its value
  TMutExtVtx::get().set_verbosity( MUTOO::SOME );

  // mMfmMT::setMapFileFlag(4);
  // mMfmMT::setMapFileScale(1.0);

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

  // read all possible nodes from the DST
  MuonReadbackDST *readback_dst = new MuonReadbackDST();
  se->registerSubsystem( readback_dst );

  // RPC
  if( use_RPC )
  {
    gSystem->Load("librpc_subsysreco");
    se->registerSubsystem( new RpcReadbackDST() );
  }

  // global reco
  se->registerSubsystem( new GlobalReco() );

  // evaluators
  if( do_evaluation ) {
    readback_dst->set_do_dbinit( true );
    se->registerSubsystem( new MuonAnaTuples() );
    if( is_mc ) se->registerSubsystem( new MuonEval() );
  }

  // qa
  if( qafile && do_qa ) {
    readback_dst->set_do_dbinit( true );

    gSystem->Load("libdstqa_muons");
    TFile *qaout = TFile::Open(qafile, "RECREATE");
    se->registerSubsystem( new QABbc() );
    se->registerSubsystem( new QAZdc() );
    se->registerSubsystem( new QAMui() );
    se->registerSubsystem( new QAMut() );
  }

  ///////////////////////////////////////////
  // Input manager
  ///////////////////////////////////////////
  Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager("IMDST","DST");
  se->registerInputManager(signal);
  se->fileopen(signal->Name(),inputfile);

  ///////////////////////////////////////////
  // Output manager
  ///////////////////////////////////////////
  /*
    nanodst setup.
    ndst module must be run when either nano or picodst is required
  */
  if( write_ndst || write_pdst ) {

    // register nanoDST subsystem
    gSystem->Load("libMWGOO");
    PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
    se->registerSubsystem(new MWGOOReco(MWGcuts));

  }

  // ndst file output
  if( ndstfile && write_ndst ) {

    Fun4AllDstOutputManager *ndstManager = new Fun4AllDstOutputManager( "MWGOUT",ndstfile );
    ndstManager->AddNode("PHGlobal");
    ndstManager->AddNode("PHGlobal_MUON");
    ndstManager->AddNode("PHMuoTracksOO");
    ndstManager->AddNode("EventHeader");
    ndstManager->AddNode("TrigLvl1");

    se->registerOutputManager(ndstManager);

  }

  // picodst setup
  if( pdstfile && write_pdst ) {

    // register nanoDST subsystem
    gSystem->Load("libMWGpico");
    MWGpico *picoDST = new MWGpico();

    // define nanoDST io manager
    picoDST->MakePico("dimuonsOO", pdstfile );
    picoDST->InitCuts("nocuts");                 // no cuts applied
    picoDST->PrintCuts();
    se->registerSubsystem(picoDST);

  }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  se->run(nEvents);
  se->End();
  cout << "Completed reconstruction." << endl;

}
