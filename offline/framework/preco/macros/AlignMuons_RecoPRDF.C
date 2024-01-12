// $Id: AlignMuons_RecoPRDF.C,v 1.5 2005/03/24 15:41:41 andryr Exp $
/*! 
  prdf analysis loop for real data
*/


void AlignMuons_RecoPRDF(
  int nEvents = 0,                   // number of events to be processed
  char *inputfile = "data.prdf",      // prdf input file
  char *ntuplename = "muon_align_ntuples.root" // name of the output ntuple file
)

{

  bool write_dst = false;
  bool use_alignfile = false;
  
  // load libraries
  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libmutoo_subsysreco.so");  // these are mutoo super modules
  //gSystem->Load("libonlreco.so");
  gSystem->Load("liblvl2.so");
  
  cerr << "libraries loaded.\n";
  
  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  

  rc->set_IntFlag("BFIELDFLAG",0);
  //rc->set_IntFlag("MUTOO_L2_VTX",1);
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);

  rc->set_IntFlag("EMBEDFLAG", 0);
  rc->set_IntFlag("GEOMFLAG", 1);

  mMfmMT::setMapFileFlag(4); 
  mMfmMT::setMapFileScale(0.0);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////
  // global detectors subsystem
  se->registerSubsystem( new HeadReco() );
  se->registerSubsystem( new TrigReco( ));
  se->registerSubsystem( new BbcReco() );
  se->registerSubsystem( new ZdcReco() );
  se->registerSubsystem( new VtxReco() );

  
  // local level1 subsystem
  TrigSelect *minBias = new TrigSelect("MB");
  minBias->AddTrigger("BBCLL1"); // run4
  // for run5, after run number 150292, one should rather use:
  //minBias->AddTrigger("BBCLL1(>0 tubes)");
  se->registerSubsystem(minBias);

  // prdf unpacker
  se->registerSubsystem( new MuonUnpackPRDF() );
  
  // muid OO reconstruction
  se->registerSubsystem( new MuiooReco() );
  
  // Global Reco
  se->registerSubsystem( new GlobalReco( GlobalReco::RUN4_AUAU ) );
  
  // Load Align module ========================
  se->registerSubsystem( new MuonAlignDev() );
  se->registerSubsystem( new MuonAlign("MUONALIGN", ntuplename) );
  //============================================
  
  ///////////////////////////////////////////
  // Write dst output
  ///////////////////////////////////////////

  if(write_dst ) {

    Fun4AllDstOutputManager *dst_MU2_Manager  = new Fun4AllDstOutputManager("DST_MU2_OUT","DST_MU2.root");
    dst_MU2_Manager->AddNode("RunHeader");
    dst_MU2_Manager->AddNode("EventHeader");
    dst_MU2_Manager->AddNode("TMutTrk");
    dst_MU2_Manager->AddNode("TMuiRoadO");
    se->registerOutputManager(dst_MU2_Manager);
				     

    Fun4AllDstOutputManager *dst_MU2_Reco_Manager  = new Fun4AllDstOutputManager("DST_MU2_Reco_OUT","DST_MU2_Reco.root");
    dst_MU2_Reco_Manager->AddNode("RunHeader");  
    dst_MU2_Reco_Manager->AddNode("EventHeader");  
    dst_MU2_Reco_Manager->AddNode("TMuiHitO");
    dst_MU2_Reco_Manager->AddNode("TMutHit");  
    se->registerOutputManager(dst_MU2_Reco_Manager);

    Fun4AllDstOutputManager *DST_MU2Clu_Manager  = new Fun4AllDstOutputManager("DST_MU2Clu","DST_MU2Clu.root");
    DST_MU2Clu_Manager->AddNode("RunHeader");  
    DST_MU2Clu_Manager->AddNode("EventHeader");
    DST_MU2Clu_Manager->AddNode("TMutClus");
    DST_MU2Clu_Manager->AddNode("TMuiClusterO");
    se->registerOutputManager(DST_MU2Clu_Manager);

  }
  
  if(use_alignfile) {  
    TMutDatabaseCntrl::print();
    TMutDatabaseCntrl::set_database_access("internal_alignment_corrections",true);
    TMutDatabaseCntrl::set_database_access("use_local_internal_align_file",true);
    TMutDatabaseCntrl::print();
  }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  pfileopen(inputfile);
  prun(nEvents);
  se->End();

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");
  cout << "Completed reconstruction." << endl;
}
