#include <cmath>
#include <TFile.h>
#include <TTree.h>
#include <cassert>
#include "SaveCanvas.C"
#include "SetOKStyle.C"

using namespace std;

TFile * _file0 = NULL;
TTree * alignment = NULL;

void
RecoAlignDST_Test(int nEvents = 10000000,
    TString infile =
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_MuTr/ZEROFDATA_P00-ALL.PRDFF.root_AlignDST.root"//
//        "/direct/phenix+user05/phnxreco/FVTX_Jin/jinhuang/miliped_work/Filter_MuTr/ZEROFDATA_P00-0000368800-0008.PRDFF.root_AlignDST.root"//
"FvtxGlobalAlign_AlignDST.root"
)
{

  if (!_file0)
    {
//    _file0 = TFile::Open(infile);
      TChain * T = new TChain("alignment");
      TString chian_str = infile;
      chian_str.ReplaceAll("ALL", "*");

      const int n = T->Add(chian_str);
      cout << "Loaded " << n << " root files with " << chian_str << endl;

      _file0 = new TFile();
      alignment = T;
    }

  assert(_file0);

  if (!alignment)
    alignment = (TTree *) _file0->GetObjectChecked("alignment", "TTree");

  // load libraries

  gSystem->Load("libfvtx_subsysreco.so");
  gSystem->Load("libfvtxgeom.so");
  gSystem->Load("libfun4all");

  cerr << "libraries loaded.\n";

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  TFvtxDatabaseCntrl::set_flag("deadmap_auto_load", false);
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path","./");

  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));
  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

  ///////////////////////////////////////////
//  //Perform alignment:
  ///////////////////////////////////////////
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();

//  fvtx_global_align->set_vtx_vertex_name("SVX_PRECISE_TEST_AVE_XY");

  fvtx_global_align->Verbosity(nEvents <= 100 ? 2 : 0);

  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS_STATIONS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS_CAGE_POS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, false);
//  fvtx_global_align->set_wedge_status(0,3,37,FvtxGlobalAlign::DET_EXCLUDE);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, false);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CONSTRAINTS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_W, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_Z, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PHI, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIX, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIY, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGNMENT, true);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, true);

  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MILLEPEDE_TRACK_FIT, false);

  fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_LATCON_FIT, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_SVTX_CONSTRAINT, true);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_VTX_HITS, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_MUTR_HITS, false);
//  fvtx_global_align->set_mutr_hit_sigma_min(0);
//  fvtx_global_align->set_mutr_hit_sigma_min(1);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_FVTX_ALONE_TRACK, false);


  fvtx_global_align->set_flag(FvtxGlobalAlign::RECAL_CONSTRAINT, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGN_DST, false);

  fvtx_global_align->set_vertex_lateral_constraint(0.0001);

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

  se->registerSubsystem(fvtx_global_align);

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  gSystem->ListLibraries();

  fvtx_global_align->minimize_alignment_DST(alignment, nEvents);

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Completed reconstruction." << endl;
}
