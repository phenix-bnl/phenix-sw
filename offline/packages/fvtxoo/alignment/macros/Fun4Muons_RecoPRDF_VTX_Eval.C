// $Id: Fun4Muons_RecoPRDF_VTX_Eval.C,v 1.1 2013/10/16 22:34:22 jinhuang Exp $
/*!
 prdf analysis loop for real data. Unpack the data,
 creates a unreconstructed DST
 */

//#include "CommonHeader.h"
using namespace std;

void
Fun4Muons_RecoPRDF_VTX_Eval(
//
//        int nEvents = 300*12*3, //
    int nEvents = 100, //
//    int nEvents = 300, //
//    int nEvents = 100000000, //
//        bool DCA_mode = true,
        bool DCA_mode = false,
    string input_file =
        "data/ZEROFDATA_P00-0000366446-0000.PRDFF", //
//    string input_file = "ZEROFDATA.FileList", //
    char *dstfile = "DST.root", //
//    char *dstfile = NULL, //
    const char *ndstfile = NULL, //
    const bool write_dst_reader = false, //
   bool IncMuTr = false)
{
  bool UseVTXForFVTX = !DCA_mode;

  cout << "Fun4Muons_RecoPRDF - alignment filtering script "
      << " with UseVTXForFVTX =" << UseVTXForFVTX << " and IncMuTr =" << IncMuTr
      <<". Extract DCA = "<<DCA_mode
      << endl;

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
  // MuTr Stuff
  ////////////////////////////////////

  if (IncMuTr)
    {

      TMutDatabaseCntrl::set_database_access("use_local_internal_align_file",
          true);
      TMutDatabaseCntrl::set_filename("use_local_internal_align_file",
          "mut.internalAligConsts_Update.dat");

      TMutDatabaseCntrl::set_verbosity(TMutDatabaseCntrl::INTERNAL_ALIGN,
          TMutDatabaseCntrl::MAX);
      TMutDatabaseCntrl::set_verbosity(TMutDatabaseCntrl::GLOBAL_ALIGN,
          TMutDatabaseCntrl::MAX);

      // muon prdf unpacker
      MuonUnpackPRDF* muon_unpack_prdf(new MuonUnpackPRDF());
//      muon_unpack_prdf->Verbosity(0);
//      muon_unpack_prdf->set_flag(MuonUnpackPRDF::SKIP_ZERO_SUPPRESSION, 1);
      se->registerSubsystem(muon_unpack_prdf);

      // mutoo reconstruction
      se->registerSubsystem(new MuiooReco());
      se->registerSubsystem(new MuonDev());
    }

  ////////////////////////////////////
  // VTX Stuff
  ////////////////////////////////////

  // SVX reconstruction"

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);

//  //  VTX Run12 alignment - Preview from Mike M. - V7
//  svxpar->set_OffsetVtxToCnt(-0.124,-0.279,0.0); // all Cu+Au
//  svxpar->set_OffsetEastToWest(0.037,0.011,0.0); // all Cu+Au
//  svxpar->set_BeamCenter(0.1633,-0.1387); // run 375549 only
//
//  svxpar->set_ReadGeoParFromFile(1);
//  svxpar->set_GeometryFileName(
//      "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/svxPISA.par_v7_Sept2013_mccumber");
//  se->registerSubsystem(svxpar);

  //  VTX Run12 alignment - Preview from Mike M. - V10
  svxpar->set_OffsetVtxToCnt(-0.124,-0.279,0.0);
  svxpar->set_OffsetEastToWest(0.037,0.011,0.0);
  svxpar->set_BeamCenter(0.1643,-0.1373);

  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_GeometryFileName(
      "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/svxPISA.par_v10_Oct2013_mccumber");
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




  ////////////////////////////////////
  // FVTX Stuff
  ////////////////////////////////////

  //  TFvtxDatabaseCntrl::set_flag("deadmap_auto_load", true);
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path", "./");

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

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->set_use_svx_cluster(UseVTXForFVTX); // Uses smeared PISA hits if false
//  fvtxreco->set_finder(1);
  fvtxreco->set_do_mutr_matching(IncMuTr);
  fvtxreco->set_fvtx_mutr_proximity_cut(10);
  se->registerSubsystem(fvtxreco);

//  if (nEvents <= 100)
//    se->registerSubsystem(new FvtxEval());

  FvtxPrimVertex* fvtxprimvtx7 = new FvtxPrimVertex();
  fvtxprimvtx7->set_source( FvtxPrimVertex::Segments );
  fvtxprimvtx7->set_clustering( FvtxPrimVertex::AllInOne );
  fvtxprimvtx7->set_vertex_output("TEST_VTX_ONLY_1",111,"TEST_VTX_ONLY_2",112);
  se->registerSubsystem(fvtxprimvtx7);


  //
  //  //Perform alignment:
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();

  fvtx_global_align->set_vtx_vertex_name("TEST_VTX_ONLY_1");

  //  fvtx_global_align->load_beam_xy_data("BeamPos.dat");

  fvtx_global_align->Verbosity(nEvents <= 100 ? 2 : 0);

  //  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, true);
  //  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, false);
  //  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, false);
  //  fvtx_global_align->set_wedge_status(0,3,37,FvtxGlobalAlign::DET_EXCLUDE);

  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_W, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_Z, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PHI, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIX, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIY, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_EVALUATION, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::WRITE_MEM, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::READ_MEM, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGNMENT, false);

  //  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, true);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
  //  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, DCA_mode);

  //  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
  //  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, true);
  fvtx_global_align->set_mutr_hit_sigma_min(0);
  fvtx_global_align->set_svx_hit_sigma(5);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, DCA_mode);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_ALONE_TRACK, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, !DCA_mode);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, false);

  fvtx_global_align->set_vertex_lateral_constraint(0.0001);

  se->registerSubsystem(fvtx_global_align);

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

      dstManager->AddNode("VtxOut");
      dstManager->AddNode("SvxSegmentList");

      se->registerOutputManager(dstManager);
    }
  // dst
  if (ndstfile)
    {

      // MWG
      gSystem->Load("libMWGOO");
      gSystem->Load("libmutrg");
      gSystem->Load("libnanoDST"); // 03Feb2013 beaumier Needed for pDST(?)
      gSystem->Load("librpc_subsysreco");
      gSystem->Load("librpc_muotrackreco");
      gSystem->Load("libMWGpico"); // 03Feb2013 beaumier Needed for pDST (doens't work)

      // global Reco
      SubsysReco *global = new GlobalReco();
      SubsysReco *global_muons = new GlobalReco_muons();
      //        SubsysReco *rpcmuoreco = new RpcMuoReco();

      se->registerSubsystem(global);
      se->registerSubsystem(global_muons);
      //        se->registerSubsystem(rpcmuoreco);

      PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
      MWGcuts->set_dodimu(false);
      MWGFvtxReco * mwgreco = new MWGFvtxReco(MWGcuts);
      //        MWGOOReco * mwgreco = new MWGOOReco(MWGcuts);
      se->registerSubsystem(mwgreco);

      gSystem->Load("libpicodst_object.so");
      mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
      msngl->set_bbcz_cut(150.0);
      msngl->set_is_sim(true);
      se->registerSubsystem(msngl);

      //********************* Mutr MC information Module ******************//
      mFillMCSingleMuonContainer* msngl_mc = new mFillMCSingleMuonContainer();
      se->registerSubsystem(msngl_mc);

      mFillDiMuonContainer* mdi = new mFillDiMuonContainer(false, 2.0); // 1st parameter: with mixed even?, 2nd parameter = Z vertex resolution
      se->registerSubsystem(mdi);
      mdi->set_bbcz_cut(100);
      mdi->set_mass_cut(0.5);
      // mdi->set_only_same_arm(false); // true for cosmics and Z analysis
      mdi->set_is_pp(true);

      if (write_dst_reader)
        {
          mDSTReader * dr = new mDSTReader();
          //          dr->Verbosity(2);
          dr->add_node(mDSTReader::FvtxTrk);
          dr->add_node(mDSTReader::MutTrk);
          //
          se->registerSubsystem(dr);
        }

      Fun4AllDstOutputManager *dstManager = new Fun4AllDstOutputManager(
          "DSTOUT", ndstfile);

      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");

      dstManager->AddNode("SingleMuonContainer");
      dstManager->AddNode("MCSingleMuonContainer"); // Mutr MC information container
      dstManager->AddNode("DiMuonContainer");

      dstManager->AddNode("VtxOut");
      dstManager->AddNode("PHGlobal");
      dstManager->AddNode("PHGlobal_MUON");

      dstManager->AddNode("PHPythiaHeader");
      dstManager->AddNode("PHPythia");

      if (write_dst_reader)
        dstManager->AddNode("DSTReader");

      dstManager->AddNode("PHMuoTracksOO");

      //      dstManager->AddNode("TMutTrk");
      //      dstManager->AddNode("TFvtxTrk");

      dstManager->AddEventSelector("mFillSingleMuonContainer");

      se->registerOutputManager(dstManager);
    }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  gSystem->ListLibraries();
  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(input_file);
//  in->AddListFile(input_file);
  se->registerInputManager(in);
  se->run(nEvents);
  se->End();

  cout << "Completed reconstruction." << endl;
}
