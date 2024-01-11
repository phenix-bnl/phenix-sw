// $Id: Fun4Muons_RecoDST_Test_Old.C,v 1.1 2013/10/16 22:34:21 jinhuang Exp $
/*!
 prdf analysis loop for real data. Unpack the data,
 creates a unreconstructed DST
 */

//#include "CommonHeader.h"
using namespace std;

void
Fun4Muons_RecoDST_Test( //
//        int nEvents = 0, //
//    int nEvents = 10000000, //
        int nEvents = 300, //
//    int nEvents = 10000, //
//    string input_file =    "data/367607/367607.FileList", //
    string input_file = "DST.FileList", //
    char *dstfile = "FvtxGlobalAlign_DST.root", //
    char *ana_file = "muon_ana_ntuples.root" //
    )
{

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

//  \brief   load disalignments from file, update fvtx geometry consequently
//           Call FvtxDisalign::add_corrections(file_name) as much as u want
//           Then call FvtxDisalign("FVTXDISALIGN")
  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));
  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

  // fvtx prdf unpacker
  FvtxUnpackDST * unpack = new FvtxUnpackDST();
  unpack->SetMode(FvtxUnpackDST::REAL_SIGNAL_NO_BG);
  se->registerSubsystem(unpack);

  FvtxReco* fvtxreco = new FvtxReco();
  fvtxreco->run_after_burner(false); // Adds VTX hits to tracking
  fvtxreco->set_use_svx_cluster(false); // Uses smeared PISA hits if false
  fvtxreco->set_do_mutr_matching(false);
  se->registerSubsystem(fvtxreco);

//
//  //Perform alignment:
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();

  fvtx_global_align->load_beam_xy_data();

  fvtx_global_align->Verbosity(nEvents <= 100 ? 2 : 0);

  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, false);

  fvtx_global_align->set_wedge_status(0,3,37,FvtxGlobalAlign::DET_EXCLUDE);


//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_W, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_Z, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PHI, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIX, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIY, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGN_DST, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::WRITE_MEM, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::READ_MEM, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, true);


  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, true);






//  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
//    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
//      for (int station = 0; station < FVTXGEOM::NumberOfStations; station+=3)
//	for (int sector = 6; sector < FVTXGEOM::NumberOfSectors; sector+=12)
//	  {
//	    fvtx_global_align->fix_fvtx_wedge(
//					      arm, cage, station, sector,
//					      FvtxGlobalAlign::PAR_Z | FvtxGlobalAlign::PAR_W | FvtxGlobalAlign::PAR_PHI);
//	  }

//  const int fix = FvtxGlobalAlign::PAR_Z | FvtxGlobalAlign::PAR_W
//      | FvtxGlobalAlign::PAR_PHI;
//
//  int arm, station;
//
//  if (0)
//    {
//      // select a subset of wedge for constrant
//
//      arm = 0;
//      station = 0;
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 3, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 6, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 10, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 14, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 17, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 19, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 0, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 1, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 5, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 8, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 12, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 16, fix);
//
//      arm = 1;
//      station = 0;
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 2, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 6, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 13, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 18, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 21, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 22, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 6, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 8, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 15, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 18, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 20, fix);
//    }
//
//  if (1)
//    {
//
//      arm = 0;
//      station = 3;
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 2, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 3, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 4, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 5, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 9, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 15, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 17, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 18, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 19, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 0, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 3, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 5, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 12, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 17, fix);
//
//      arm = 1;
//      station = 3;
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 2, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 7, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 11, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 13, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 18, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 21, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 0, station, 22, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 6, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 15, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 20, fix);
//      fvtx_global_align->fix_fvtx_wedge(arm, 1, station, 23, fix);
//    }

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
//      dstManager->AddNode("TFvtxAlignPar");
      //      dstManager->AddNode("TFvtxTrk");
//      dstManager->AddNode("TFvtxCoord");

      se->registerOutputManager(dstManager);
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

  cout << "Completed reconstruction." << endl;
}
