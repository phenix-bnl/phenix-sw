// $Id: Fun4Muons_RecoPRDF_AlignTest.C,v 1.1 2013/10/16 22:34:22 jinhuang Exp $
/*!
 prdf analysis loop for real data. Unpack the data,
 creates a unreconstructed DST
 */

//#include "CommonHeader.h"
using namespace std;

void
Fun4Muons_RecoPRDF_Test( //
//        int nEvents = 400000, //
//    int nEvents = 100, //
    int nEvents = 3000, //
//    int nEvents = 20, //
//    string input_file =    "data/367607/367607.FileList", //
    string input_file = "RunList", //
    char *dstfile = "FvtxGlobalAlign_DST.root", //
    char *ana_file = "muon_ana_ntuples.root" //
    )
{

  // flags
  bool use_lvl2 = false;

  // load libraries
//  gSystem->Load("libfun4all");
//  gSystem->Load("libfun4allfuncs_muons");
//  gSystem->Load("libmutoo_subsysreco");
//  gSystem->Load("libfvtx_subsysreco");
//  gSystem->Load("libfvtxgeom");
//  gSystem->Load("libfvtxoo_alignment");

  // load libraries

  gSystem->Load("libfvtx_subsysreco.so");
  gSystem->Load("libfvtxgeom.so");

  gSystem->Load("libfun4all");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");
  gSystem->Load("libmutoo_subsysreco");

  cerr << "libraries loaded.\n";

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS", 1);
//  rc->set_IntFlag("RUNNUMBER", run_number);
  if (use_lvl2)
    {
      // Set Lvl2 flags
      rc->set_IntFlag("LVL2_REAL_DATA", 1);
      rc->set_IntFlag("LVL2_YEAR", 4);
      rc->set_IntFlag("FORCE_LVL2", 1);
      rc->set_IntFlag("LVL2_USE_ASCII_DB", 1);
      rc->set_CharFlag("LVL2_DB_DIR",
          "/afs/rhic.bnl.gov/phenix/users/frawley/lvl2_db/RUN4_REAL");
      rc->set_CharFlag("Run2Lvl2AlgoName", "");
    }

  // mutoo vertex source configuration
  // this allows to print which vertex is used and its value
  TMutExtVtx::get().set_verbosity(MUTOO::NONE);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////
//
  // Counter
//  se->registerSubsystem(new MuonCounter());

  // global detectors subsystem
  se->registerSubsystem(new HeadReco());
  se->registerSubsystem(new TrigReco());

  //set BBC-vtx resolution
  BbcReco *bbc_reco = new BbcReco();
  bbc_reco->setBbcVtxError(0.5);
  se->registerSubsystem(bbc_reco);

  se->registerSubsystem(new ZdcReco());
  se->registerSubsystem(new VtxReco());

  // level2 selection subsystems
  if (use_lvl2)
    {
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


  ////////////////////////////////////
  // VTX Stuff
  ////////////////////////////////////

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

  //  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  //  svxhotdead->Verbosity(0);
  //  se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  // svxrec->Load_ThresholdFile("threshold.h");
  svxrec->set_UseStripThresholdDatbase(true);
  svxrec->set_StripixelAdcSumThreshold(0);
  se->registerSubsystem(svxrec);

  SvxPriVertexSeedFinder *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  svxvtxseedfinder->Verbosity(0);
  se->registerSubsystem(svxvtxseedfinder);

  SvxPriVertexSeedFinder *vtxseedw = new SvxPriVertexSeedFinder(
      "SVXPRIVERTEXFINDERW", 1);
  se->registerSubsystem(vtxseedw);

  SvxPriVertexSeedFinder *vtxseede = new SvxPriVertexSeedFinder(
      "SVXPRIVERTEXFINDERE", 2);
  se->registerSubsystem(vtxseede);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);
  svxstandalone->setWindowScale(10);
  se->registerSubsystem(svxstandalone);

  SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

//  // muon prdf unpacker
//  MuonUnpackPRDF* muon_unpack_prdf(new MuonUnpackPRDF());
//  muon_unpack_prdf->Verbosity(0);
//  muon_unpack_prdf->set_flag(MuonUnpackPRDF::SKIP_ZERO_SUPPRESSION, 1);
//  se->registerSubsystem(muon_unpack_prdf);
//
//  // mutoo reconstruction
//  se->registerSubsystem( new SvxReco() );
//  se->registerSubsystem(new MuiooReco());
//  se->registerSubsystem(new MuonDev());

  ////////////////////////////////////
  // FVTX Stuff
  ////////////////////////////////////

  FvtxGeom::set_public_file_path("./");

//  \brief   load disalignments from file, update fvtx geometry consequently
//           Call FvtxDisalign::add_corrections(file_name) as much as u want
//           Then call FvtxDisalign("FVTXDISALIGN")
  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));

  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

  // fvtx prdf unpacker
//  rc->set_IntFlag("VERBOSITY_FVTX_UNPACK", 1);
  SubsysReco *fvtx_unpack = new FvtxUnpackPRDF();
  fvtx_unpack->Verbosity(0);
  se->registerSubsystem(fvtx_unpack);
//  rc->set_CharFlag("SAVE_FVTX_UNPACK_ONLINE_DEBUG_TREE",
//      string(input_file) + ".Fun4Muons_RecoPRDF_Test.FvtxUnpack.root");

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->run_after_burner(false); // Adds VTX hits to tracking
  fvtxreco->set_use_svx_cluster(false); // Uses smeared PISA hits if false
  fvtxreco->set_do_mutr_matching(false);
  se->registerSubsystem(fvtxreco);

  // Perform FVTX-Mutr track matching and refit track:
  //se->registerSubsystem( new FvtxRecoWithMut() );

//  //Perform alignment:
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_W, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_Z, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PHI, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIX, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIY, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGN_DST, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::WRITE_MEM, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::READ_MEM, false);

//  fvtx_global_align->Verbosity(0);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, true);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_1D_Fit, true);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, true);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_VZCON_Fit, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, true);

  fvtx_global_align->set_vertex_lateral_constraint(0.0001);

  fvtx_global_align->Verbosity(nEvents<=100?2:0);

  se->registerSubsystem(fvtx_global_align);

// disable off-trend wedges
//  fvtx_global_align->set_wedge_status(0,2,37,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,3,37,FvtxGlobalAlign::DET_EXCLUDE);

//  fvtx_global_align->set_wedge_status(0,0,0,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,1,0,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,2,0,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,3,0,FvtxGlobalAlign::DET_EXCLUDE);
//
//  fvtx_global_align->set_wedge_status(0,0,24,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,1,24,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,2,24,FvtxGlobalAlign::DET_EXCLUDE);
//  fvtx_global_align->set_wedge_status(0,3,24,FvtxGlobalAlign::DET_EXCLUDE);


  //FvtxMCEval* fvtxeval = new FvtxMCEval();

//  string evl_file = input_file + ".Fun4Muons_RecoPRDF_Test.fvtx_eval.root";
//  FvtxEval* fvtxeval = new FvtxEval("FvtxEval", evl_file.c_str());
//  se->registerSubsystem(fvtxeval);

  //se->registerSubsystem( new MuonAnaTuples(ana_file) );

  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////

  // dst
  if (dstfile)
    {
      Fun4AllDstOutputManager *dstManager = new Fun4AllDstOutputManager(
          "DSTOUT", dstfile);

      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");
//      dstManager->AddNode("VtxOut");
//      dstManager->AddNode("BbcOut");
//      dstManager->AddNode("BbcRaw");
//      dstManager->AddNode("ZdcOut");
//      dstManager->AddNode("ZdcRaw");
//      dstManager->AddNode("TrigLvl1");
//      dstManager->AddNode("L2Decision");
//      dstManager->AddNode("Lvl2OutArray");

      // Muioo nodes
//      dstManager->AddNode("TMuiHitO");

      // Mutoo nodes
      //      dstManager->AddNode("TMutHit");
      dstManager->AddNode("TFvtxAlignPar");
//      dstManager->AddNode("TFvtxTrk");
//      dstManager->AddNode("TFvtxCoord");

      se->registerOutputManager(dstManager);
    }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
//  in->fileopen(inputfile);
  in->AddListFile(input_file);
  se->registerInputManager(in);
  se->run(nEvents);
  se->End();

  FvtxGeom::save_root_geometry();

  cout << "Completed reconstruction." << endl;
}
