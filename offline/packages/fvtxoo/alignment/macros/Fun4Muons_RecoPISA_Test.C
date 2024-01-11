// $Id: Fun4Muons_RecoPISA_Test.C,v 1.1 2013/10/16 22:34:21 jinhuang Exp $
/*!
 prdf analysis loop for real data. Unpack the data,
 creates a unreconstructed DST
 */

//#include "CommonHeader.h"
using namespace std;

void
Fun4Muons_RecoPISA_Test(
    //
//		               int nEvents = 5, //
    int nEvents = 10000000, //
//    const int nEvents = 99, //
//    int nEvents = 3000, //
//    string input_file =    "data/367607/367607.FileList", //
    //    string input_file = "PISA.FileList", //
//    const string input_file = "/phenix/u/jinhuang/links/fvtx_tmp/miliped_work/Simulation/MB_ZF_MuonTrig_VRMS10_Reco/MB_ZF_MuonTrig_VRMS10_1198_PISAEvent.root", //
    const string input_file = "/phenix/u/jinhuang/work/FVTX/miliped_work/Simulation/PISA_PartGun/PISAEvent_9-15GeV_mu+-_10cmVzRMS.root", //
//    string input_file = "DST.FileList.Local", //
//    char *dstfile = "FvtxGlobalAlign_DST.root", //
    const char *dstfile = NULL, //Local", //
    const char *ndstfile = "FvtxGlobalAlign_nDST.root",//
    const bool write_dst_reader = false //
    )
{
  int run_number = 386885;

  bool UseVTXForFVTX = false;
  bool IncMuTr = true;

  // load libraries

  gSystem->Load("libPHGeant");
  gSystem->Load("libfun4all");
  gSystem->Load("libsimreco_base");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libmuon_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("libfvtx_subsysreco");
  gSystem->Load("liblvl2");
  gSystem->Load("libPythia6.so");
  gSystem->Load("libPHPythia.so");
  gSystem->Load("libPHPythiaEventGen.so");
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

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", 1);

  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS", 1);
  rc->set_IntFlag("RUNNUMBER", run_number);

//  TMutExtVtx::get().set_verbosity(MUTOO::SOME);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  // run header and trigger setting
  se->registerSubsystem(new HeadSimreco());
  //    se->registerSubsystem( new TrigSimreco() );

  // vertex simulation
  // puts the vertex from the pisa header node into vtxOut object
  if (true)
    {
      gSystem->Load("libsimreco_base.so");
      VtxSimreco* vtx_reco = new VtxSimreco();
      vtx_reco->SmearZ(true);
      vtx_reco->UseXY(false);
      vtx_reco->OverwriteBBC(true);
      vtx_reco->ZVertexSigma(0.00001);
//      vtx_reco->ZVertexSigma(1.00);
      se->registerSubsystem(vtx_reco);
    }

  // Counter
  MuonCounter * muc = new MuonCounter();
  muc -> set_log_scale_dump();
  se->registerSubsystem(muc);

  // global detectors subsystem
  //se->registerSubsystem( new HeadReco() );
  //se->registerSubsystem( new TrigReco( ));
  //se->registerSubsystem( new BbcReco() );
  //se->registerSubsystem( new ZdcReco() );
  se->registerSubsystem(new VtxReco());

  // muon prdf unpacker
  MuonUnpackPisa* muon_unpack_pisa(new MuonUnpackPisa());
  muon_unpack_pisa->Verbosity(1);
  muon_unpack_pisa->set_flag(MuonUnpackPRDF::SKIP_ZERO_SUPPRESSION, 1);
  se->registerSubsystem(muon_unpack_pisa);

  // mutoo reconstruction
  se->registerSubsystem(new MuiooReco());
  MuonDev *muon_dev = new MuonDev();
  muon_dev->set_flags(MuonDev::FORCE_LOOKUP);
  se->registerSubsystem(muon_dev);

  // SVX reconstruction"

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  svxpar->set_BeamCenter(0., 0.);
  svxpar->set_OffsetVtxToCnt(0.0, 0.0, 0.0);
  svxpar->set_OffsetEastToWest(0.0, 0.0, 0.0);
  svxpar->set_ReadGeoParFromFile(1);
  svxpar->set_GeometryFileName(
      "/afs/rhic.bnl.gov/phenix/users/jinhuang/public/FVTX/run12_pp_510/simulation/svxPISA.par.ideal");
  se->registerSubsystem(svxpar);

  SvxSimulator *svxsim = new SvxSimulator();
  svxsim->Verbosity(0);
  se->registerSubsystem(svxsim);

  /*
   SvxDecode *svxdecode = new SvxDecode();
   svxdecode->Verbosity(0);
   svxdecode->includePixel(true);
   svxdecode->includeStripixel(true);
   svxdecode->setAdcOffset(24);
   svxdecode->setAdcCutoff(-24);
   se->registerSubsystem(svxdecode);
   */

  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  svxhotdead->Verbosity(0);
  se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  // svxrec->Load_ThresholdFile("threshold.h");
  svxrec->set_ThisIsSimulation();
  svxrec->set_UseStripThresholdDatbase(true);
  //svxrec->Load_ThresholdFile("threshold_ideal.h");
  svxrec->set_StripixelAdcSumThreshold(0);
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

//
  ////////////////////////////////////
  // FVTX Stuff
  ////////////////////////////////////

  TFvtxDatabaseCntrl::set_flag("is_sim", true);
  TFvtxDatabaseCntrl::set_flag("deadmap_auto_load", false);
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);

//  \brief   load disalignments from file, update fvtx geometry consequently
//           Call FvtxDisalign::add_corrections(file_name) as much as u want
//           Then call FvtxDisalign("FVTXDISALIGN")
  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));
  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

  FvtxUnpackPisa *fvtx_unpack = new FvtxUnpackPisa();
  fvtx_unpack->set_do_response(true);
  fvtx_unpack->Verbosity(0);
  se->registerSubsystem(fvtx_unpack);

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->set_use_svx_cluster(UseVTXForFVTX); // Uses smeared PISA hits if false
//  fvtxreco->set_finder(1);
  fvtxreco->set_do_mutr_matching(IncMuTr);
  fvtxreco->set_fvtx_mutr_proximity_cut(10);
  se->registerSubsystem(fvtxreco);

//  FvtxReco* fvtxreco = new FvtxReco();
//  fvtxreco->run_after_burner(false); // Adds VTX hits to tracking
//  fvtxreco->set_use_svx_cluster(true); // Uses smeared PISA hits if false
//  fvtxreco->set_do_mutr_matching(true); // Match FVTX-MuTr tracks and refit or not
//  fvtxreco->set_auto_load_dead_map(false);
//  fvtxreco->set_fvtx_mutr_proximity_cut(10);
////  fvtxreco->set_fvtx_mutr_proximity_cut(0);
//  se->registerSubsystem(fvtxreco);

//  if (nEvents <= 100)
//    se->registerSubsystem(new FvtxEval());

//
//  //Perform alignment:
//  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();
//
////  fvtx_global_align->load_beam_xy_data("BeamPos.dat");
//
//  fvtx_global_align->Verbosity(nEvents <= 100 ? 2 : 0);
//
////  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, true);
////  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, false);
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, false);
////  fvtx_global_align->set_wedge_status(0,3,37,FvtxGlobalAlign::DET_EXCLUDE);
//
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_W, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_Z, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PHI, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIX, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIY, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_EVALUATION, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::WRITE_MEM, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::READ_MEM, false);
//
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGNMENT, false);
//
////  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, true);
//
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, false);
//
//  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, true);
//
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, true);
//  fvtx_global_align->set_mutr_hit_sigma_min(0);
//  fvtx_global_align->set_svx_hit_sigma(.5);
////  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_ALONE_TRACK, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, false);
//
//  fvtx_global_align->set_vertex_lateral_constraint(0.0001);
//
//  se->registerSubsystem(fvtx_global_align);



  if (nEvents <= 100)
    se->registerSubsystem(new FvtxEval());

//  //Perform alignment:
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign("FVTX_GLOBAL_ALIGN");

//  fvtx_global_align->load_beam_xy_data();

  fvtx_global_align->Verbosity(nEvents <= 100 ? 2 : 0);

  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGN_DST, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGN_DST, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGNMENT, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, false);


//  fvtx_global_align->set_mutr_hit_sigma_min(1);
  fvtx_global_align->set_mutr_hit_sigma_min(0);
  fvtx_global_align->set_svx_hit_sigma(5);
  fvtx_global_align->set_z_ref(110);
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

      dstManager->AddNode("TMutTrk");
      dstManager->AddNode("TFvtxTrk");

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
      MuonEval* mueval = new MuonEval();
      mueval->set_flags((1<<2) );
      se->registerSubsystem(mueval);

      mFillMCSingleMuonContainer* msngl_mc = new mFillMCSingleMuonContainer();
      se->registerSubsystem(msngl_mc);

      if (write_dst_reader)
        {
          mDSTReader * dr = new mDSTReader();
//          dr->Verbosity(2);
          dr->add_node(mDSTReader::FvtxTrk);
          dr->add_node(mDSTReader::MutTrk);
          dr->add_node(mDSTReader::MutMCTrk);
//
          se->registerSubsystem(dr);
        }

      Fun4AllDstOutputManager *dstManager = new Fun4AllDstOutputManager(
          "DSTOUT", ndstfile);

      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");

      dstManager->AddNode("SingleMuonContainer");
      dstManager->AddNode("MCSingleMuonContainer"); // Mutr MC information container

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

  Fun4AllPisaInputManager *inMan = new Fun4AllPisaInputManager("PisaIn");
  se->registerInputManager(inMan);
  inMan->fileopen(input_file);
//  inMan->AddListFile(input_file);
  se->registerInputManager(inMan);

  se->run(nEvents);
  se->End();

  cout << "Completed reconstruction." << endl;
}
