// $Id: Fun4Muons_RecoDST_Test.C,v 1.1 2013/10/16 22:34:21 jinhuang Exp $
/*!
 prdf analysis loop for real data. Unpack the data,
 creates a unreconstructed DST
 */

//#include "CommonHeader.h"
using namespace std;

void
Fun4Muons_RecoDST_Test(
    //

//		               int nEvents = 5, //
//    int nEvents = 10000000, //
    int nEvents = 101, //
//    int nEvents = 99, //

    string input_file =
        "ZeroDST.FILELIST", //
//    string input_file = "DST.FileList", //

//    string input_file = "DST.FileList.Local", //
//    char *dstfile = "FvtxGlobalAlign_DST.root", //
    char *dstfile = NULL)
{

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
//
  ////////////////////////////////////
  // FVTX Stuff
  ////////////////////////////////////

    TFvtxDatabaseCntrl::set_flag("deadmap_auto_load", false);
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path", "./");

//  \brief   load disalignments from file, update fvtx geometry consequently
//           Call FvtxDisalign::add_corrections(file_name) as much as u want
//           Then call FvtxDisalign("FVTXDISALIGN")
  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));
  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

//  // fvtx prdf unpacker
//  FvtxUnpackDST * unpack = new FvtxUnpackDST();
//  unpack->SetMode(FvtxUnpackDST::REAL_SIGNAL_NO_BG);
//  se->registerSubsystem(unpack);
  MuonReadbackDST * mu_dst = new MuonReadbackDST();
  mu_dst->set_do_dbinit(true);
  se->registerSubsystem(mu_dst);
  se->registerSubsystem(new FvtxReadbackDST());

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->set_use_svx_cluster(true); // Uses smeared PISA hits if false
  fvtxreco->set_do_mutr_matching(true);
  fvtxreco->set_fvtx_mutr_proximity_cut(10);
//  fvtxreco->set_fvtx_mutr_proximity_cut(0);
  se->registerSubsystem(fvtxreco);

//  FvtxVertexTest * vtxtest = new FvtxVertexTest();
//  vtxtest->load_default_data();
//  se->registerSubsystem(vtxtest);

//  if (nEvents <= 100)
//    se->registerSubsystem(new FvtxEval());

//
//  //Perform alignment:
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();

//  fvtx_global_align->set_vtx_vertex_name("SVX_PRECISE_TEST_AVE_XY");

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

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGNMENT, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, true);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, true);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, true);
//  fvtx_global_align->set_mutr_hit_sigma_min(0);
//  fvtx_global_align->set_mutr_hit_sigma_min(1);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_ALONE_TRACK, true);

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

  const char *singlepdstout = "singlemuon_pdst.root";
  const char *dimuonpdstout = "dimuon_pdst.root";
  const bool write_pdst = false;

  if (singlepdstout && write_pdst)
    {

      // global Reco
      se->registerSubsystem(new GlobalReco());

      // MWG
      gSystem->Load("libMWGOO.so");
      PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
      se->registerSubsystem(new MWGFvtxReco(MWGcuts));

      gSystem->Load("libpicodst_object.so");

      mDSTReader * dr = new mDSTReader();
      //  dr -> Verbosity(2);
      dr->add_node(mDSTReader::FvtxCoord);
      dr->add_node(mDSTReader::FvtxTrk);
      se->registerSubsystem(dr);

      mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
      msngl->set_bbcz_cut(15.0);
      msngl->set_vtx_vertex_name("SVX_PRECISE_TEST_AVE_XY");
      se->registerSubsystem(msngl);

      Fun4AllOutputManager *outsmu = new Fun4AllDstOutputManager("Outsmu",
          singlepdstout);
      outsmu->AddNode("Sync");
      outsmu->AddNode("SingleMuonContainer");
      outsmu->AddNode("VtxOut");
      outsmu->AddNode("PHGlobal");
      outsmu->AddNode("EventHeader");
      outsmu->AddNode("DSTReader");
      outsmu->AddEventSelector("mFillSingleMuonContainer");
      se->registerOutputManager(outsmu);

      if (dimuonpdstout)
        {
          mFillDiMuonContainer* mdi = new mFillDiMuonContainer(false); // do not make mixed events
          se->registerSubsystem(mdi);
          mdi->set_mass_cut(0.5);
          mdi->set_is_pp(true);
          mdi->set_bbcz_cut(100000);

          Fun4AllOutputManager *outdimu = new Fun4AllDstOutputManager("Outdimu",
              dimuonpdstout);
          outdimu->AddNode("Sync");
          outdimu->AddNode("VtxOut");
          outdimu->AddNode("PHGlobal");
          outdimu->AddNode("EventHeader");
          outdimu->AddNode("DiMuonContainer");
          outdimu->AddEventSelector("mFillDiMuonContainer");
          outdimu->AddNode("DSTReader");
          se->registerOutputManager(outdimu);
        }
    }

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  //pfileopen(inputfile);
  //prun(nEvents);
  Fun4AllDstInputManager *in = new Fun4AllDstInputManager("PRDFin");
//  in->fileopen(input_file);
  in->AddListFile(input_file);
  se->registerInputManager(in);
  se->run(nEvents);
  se->End();

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Completed reconstruction." << endl;
}
