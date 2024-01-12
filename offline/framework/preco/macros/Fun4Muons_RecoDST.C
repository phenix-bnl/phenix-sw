// $Id: Fun4Muons_RecoDST.C,v 1.31 2009/12/03 23:54:33 hpereira Exp $
/*!
  dst analysis loop for real data dst
*/
void Fun4Muons_RecoDST(
  int nEvents = 100,
  char *inputfile = "dst.root",
  char *dstfile = "dst_out.root",
  char *ndstfile = "ndst_out.root",
  char *pdstfile = "pdst_out.root",
  char *qafile = "qa_out.root"
)
{

  // dst/ndst/pdst output
  bool write_dst = true;
  bool write_ndst = true;
  bool write_pdst = true;

  // MUTOO evaluators
  bool do_evaluation = true;

  // QA
  bool do_qa = true;

  // Profile (code timing)
  bool do_profile = false;

  // libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");

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
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);

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

  // counter
  se->registerSubsystem( new MuonCounter() );

  MuonUnpackDST *muon_unpack_dst( new MuonUnpackDST() );
  se->registerSubsystem( muon_unpack_dst );
  se->registerSubsystem( new MuiooReco() );
  se->registerSubsystem( new MuonDev() );

  /*
    global Reco must be run after the reconstruction
    but before the evalutations for all nodes/ntuples to be filled
    properly
  */
  se->registerSubsystem( new GlobalReco() );

  // evaluator
  if( do_evaluation ) se->registerSubsystem( new MuonAnaTuples() );

  // qa
  if( qafile && do_qa ) {
    // QA libraries
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

  // dst output manager
  if( dstfile && write_dst ) {
    Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",dstfile);
    se->registerOutputManager(dstManager);
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

    dstManager->AddNode("TMuiHitO");
    dstManager->AddNode("TMuiClusterO");
    dstManager->AddNode("TMui1DRoadO");
    dstManager->AddNode("TMuiRoadO");
    dstManager->AddNode("TMuiPseudoBLTO");
    dstManager->AddNode("TMuiPseudoLL1");

    dstManager->AddNode("TMutHit");
    dstManager->AddNode("TMutClus");
    dstManager->AddNode("TMutCoord");
    dstManager->AddNode("TMutGapCoord");
    dstManager->AddNode("TMutStub");
    dstManager->AddNode("TMutTrk");
    dstManager->AddNode("TMutVtx");
  }

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

  // ndst output manager
  if( ndstfile && write_ndst ) {

    Fun4AllDstOutputManager *ndstManager = new Fun4AllDstOutputManager( "MWGOUT",ndstfile );
    ndstManager->AddNode("RunHeader");
    ndstManager->AddNode("VtxOut");
    ndstManager->AddNode("PHGlobal");
    ndstManager->AddNode("PHGlobal_MUON");
    ndstManager->AddNode("PHMuoTracksOO");
    ndstManager->AddNode("EventHeader");
    ndstManager->AddNode("TrigLvl1");
    ndstManager->AddNode("TMuiPseudoBLTO");
    ndstManager->AddNode("TMuiPseudoLL1");

    se->registerOutputManager(ndstManager);

  }

  // picodst output manager
  if( pdstfile && write_pdst ) {

    // register nanoDST subsystem
    gSystem->Load("libMWGpico");
    MWGpico *picoDST = new MWGpico();

    // define nanoDST io manager
    picoDST->MakePico("dimuonsOO", pdstfile );
    picoDST->MakePico("histograms", pdstfile );
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
