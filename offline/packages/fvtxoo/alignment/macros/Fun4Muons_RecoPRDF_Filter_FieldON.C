
//#include "CommonHeader.h"
using namespace std;

void
Fun4Muons_RecoPRDF_Filter_FieldON(
//
//    int nEvents = 40, //
    int nEvents = 100000000, //
    string input_file =
        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_Run13_HighPz_FieldON_1//GLOBAL_ALIGN_PRDF_run13_fvtx_fvtx-0000398028.filelist", //
//    string input_file = "ZEROFDATA_P00-0000368800-0010.FileList", //
    const char *dstfile = "DST.root", //
    const char * singlepdstout = "singlemu_dst.root", //
    bool UseVTXForFVTX = false, bool IncMuTr = true)
{

  cout << "Fun4Muons_RecoPRDF - alignment filtering script "
      << " with UseVTXForFVTX =" << UseVTXForFVTX << " and IncMuTr =" << IncMuTr
      << endl;


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
  MuonCounter * muc = new MuonCounter();
  muc -> set_log_scale_dump();
  se->registerSubsystem(muc);

  // global detectors subsystem
  se->registerSubsystem(new HeadReco());
  se->registerSubsystem(new TrigReco());

  //set BBC-vtx resolution
  BbcReco *bbc_reco = new BbcReco();
  bbc_reco->setBbcVtxError(2.);
  se->registerSubsystem(bbc_reco);

  se->registerSubsystem(new ZdcReco());
  se->registerSubsystem(new VtxReco());



//   // local level1 subsystem
//   TrigSelect *minBias = new TrigSelect("MB");
//   minBias->AddTrigger("BBCLL1");
//   se->registerSubsystem(minBias);

  ////////////////////////////////////
  // MuTr Stuff
  ////////////////////////////////////

  if (IncMuTr)
    {
//      TMutDatabaseCntrl::set_database_access("use_local_internal_align_file", true);
//      TMutDatabaseCntrl::set_filename("use_local_internal_align_file","mut.internalAligConsts_Update.dat");
//
//      TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN, TMutDatabaseCntrl::MAX );
//      TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN, TMutDatabaseCntrl::MAX );

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
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path","./");

  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));
  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

  // fvtx prdf unpacker
//  rc->set_IntFlag("VERBOSITY_FVTX_UNPACK", 1);
  SubsysReco *fvtx_unpack = new FvtxUnpackPRDF();
  fvtx_unpack->Verbosity(0);
  se->registerSubsystem(fvtx_unpack);

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->set_use_svx_cluster(UseVTXForFVTX); // Uses smeared PISA hits if false
  fvtxreco->set_do_mutr_matching(IncMuTr);
  fvtxreco->set_fvtx_mutr_proximity_cut(10);
  se->registerSubsystem(fvtxreco);

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

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, false);

  fvtx_global_align->set_pz_min(9);
  fvtx_global_align->set_pz_max(30);
  fvtx_global_align->set_vertex_acceptance(30);
  fvtx_global_align->set_z_ref(110);
  fvtx_global_align->set_mutr_hit_sigma_min(0);
//  fvtx_global_align->set_mutr_hit_sigma_min(10);
  fvtx_global_align->set_vertex_lateral_constraint(0.0001);

  se->registerSubsystem(fvtx_global_align);

  ///////////////////////////////////////////
  // IOManagers...
  ///////////////////////////////////////////

  // picoDST
  if (singlepdstout)
    {
      //      se->registerSubsystem( new MpcReco() );
      //      gSystem->Load("librxnp_subsysreco.so");
      //      se->registerSubsystem( new RxnpReco() );
      //      se->registerSubsystem( new RpSumXYReco() ); // recalibrator and rp doesn't work together!

      // global Reco
      se->registerSubsystem(new GlobalReco());

      // MWG
      gSystem->Load("libMWGOO.so");
      PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
      se->registerSubsystem(new MWGFvtxReco(MWGcuts));

      gSystem->Load("libpicodst_object.so");
      mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
      msngl->set_bbcz_cut(150.0);
      se->registerSubsystem(msngl);

//      if (write_dst_reader)
//        {
//          gSystem->Load("libpicodst_object.so");
//          mDSTReader * dr = new mDSTReader();
////          dr->Verbosity(2);
//          dr->add_node(mDSTReader::FvtxTrk);
//          dr->add_node(mDSTReader::FvtxCoord);
//          se->registerSubsystem(dr);
//        }

      Fun4AllOutputManager *outsmu = new Fun4AllDstOutputManager("Outsmu",
          singlepdstout);
      outsmu->AddNode("Sync");

      outsmu->AddNode("SingleMuonContainer");

      outsmu->AddNode("VtxOut");
      outsmu->AddNode("PHGlobal");

      outsmu->AddNode("PHPythiaHeader");
      outsmu->AddNode("PHPythia");

      outsmu->AddEventSelector("mFillSingleMuonContainer");

      se->registerOutputManager(outsmu);

    }

  // dst
  if (dstfile)
    {
      Fun4AllDstOutputManager *dstManager = new Fun4AllDstOutputManager(
          "DSTOUT", dstfile);

      dstManager->AddEventSelector("FVTX_GLOBAL_ALIGN");

      dstManager->AddNode("RunHeader");
      dstManager->AddNode("EventHeader");

      if (IncMuTr)
        {
          dstManager->AddNode("TMutCoord");
          dstManager->AddNode("TMutTrk");
          dstManager->AddNode("TMutVtx");
          dstManager->AddNode("TMuiRoadO");
        }

      if (UseVTXForFVTX)
        {
          dstManager->AddNode("SvxClusterList");
        }

      dstManager->AddNode("VtxOut");

      dstManager->AddNode("TFvtxHit");

      se->registerOutputManager(dstManager);
    }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
//  in->fileopen(input_file);
  in->AddListFile(input_file);
  se->registerInputManager(in);
  se->run(nEvents);
  se->End();

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Completed reconstruction." << endl;
}
