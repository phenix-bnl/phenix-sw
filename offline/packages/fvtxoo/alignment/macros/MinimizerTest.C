
//#include "CommonHeader.h"
using namespace std;

void
MinimizerTest( //
//    int nEvents = 400000
//    int nEvents = 1
    int nEvents = 0
    )
{

  // load libraries

  gSystem->Load("libfvtx_subsysreco.so");
  gSystem->Load("libfvtxgeom.so");

  gSystem->Load("libfun4all");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("liblvl2");
  gSystem->Load("libmutoo_subsysreco");

  cerr << "libraries loaded.\n";



//  //Perform alignment:
  FvtxGlobalAlign* fvtx_global_align = new FvtxGlobalAlign();
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_STATION, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_FVTX_WEDGE, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_W, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_Z, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PHI, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIX, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::ALIGN_PSIY, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::DO_ALIGN_DST, true);
  fvtx_global_align->set_flag(FvtxGlobalAlign::WRITE_MEM, false);
  fvtx_global_align->set_flag(FvtxGlobalAlign::READ_MEM, false);

  fvtx_global_align->Verbosity(0);
//  fvtx_global_align->Verbosity(1);
  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, true);
//  fvtx_global_align->set_flag(FvtxGlobalAlign::USE_CUTS, false);

//  fvtx_global_align->set_flag(FvtxGlobalAlign::ITERATE, false);
    fvtx_global_align->set_flag(FvtxGlobalAlign::TRACK_1D_Fit, true);

//    for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
//      for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
//        for (int station = 0; station < FVTXGEOM::NumberOfStations; station+=3)
//          for (int sector = 6; sector < FVTXGEOM::NumberOfSectors; sector+=12)
//            {
//              fvtx_global_align->fix_fvtx_wedge(
//                  arm, cage, station, sector,
//                  FvtxGlobalAlign::PAR_Z | FvtxGlobalAlign::PAR_W | FvtxGlobalAlign::PAR_PHI);
//            }


  fvtx_global_align->minimizer_tests(nEvents);

  cout << "Completed reconstruction." << endl;
}
