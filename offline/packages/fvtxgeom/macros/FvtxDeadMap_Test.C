// $Id: FvtxDeadMap_Test.C,v 1.4 2016/03/10 19:09:26 jinhuang Exp $                                                                                             

/*!
 * \file FvtxDeadMap_Test.C
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.4 $
 * \date $Date: 2016/03/10 19:09:26 $
 */

//#include "SaveCanvas.C"
//#include "SetOKStyle.C"
#include <cmath>
#include <iostream>
#include <algorithm>
#include <ctype.h>
#include <string>
#include <climits>
#include <cstdlib>
#include <string>
#include <sstream>
#include <fstream>
#include <cstring>
#include <cassert>

using namespace std;

void
FvtxDeadMap_Test(void)
{

  gSystem->Load("libfvtxgeom.so");

//  TFvtxDeadMap::example_deadchan2calib_db(367465);
//
//    TFvtxDeadMap::example_fvtxdb2calib_db(367465);
//
//  TFvtxDeadMap::example_print_calib_db(367465);

//  TFvtxDeadMap::example_make_dummy_deadmap();
//  UploadDeadMap_Sim();
//  UploadDeadMap_RunPeriodRun12pp();
//  UploadDeadMap_RunSingleRun12("FvtxDeadMap.pp_510.list");
//  UploadDeadMap_RunSingleRun12("FvtxDeadMap.cuau_200.list");

//  TextImport();
//  ReadTest();
//  QA_Stat();

//  UploadDeadMap_RunPeriodRun15();
//  UploadDeadMap_RunPeriodRun14();
}

int
ExportMap(int run = 392098)
{

  gSystem->Load("libfvtxgeom.so");

  TFvtxDeadMap map;
  map.dbGetAll(run);
  const int record_cnt = map.txtPutAll("fvtx.map");

  return record_cnt;
}

void
UploadDeadMap_RunSingleRun12(const char * run_list)
{
  fstream f(run_list, ios_base::in);
  assert(f.is_open());

  string line;

  while (getline(f, line))
    {
      istringstream line_in(line);

      int run = 0;
      line_in >> run;

      cout << "Get run " << run << endl;
      if (run > 0)
        {

          TFvtxDeadMap::example_fvtxdb2calib_db(run);

        }
    }

  f.close();

}

void
UploadDeadMap_Sim()
{

  TFvtxDeadMap map;

  map.load_dead_chan_map_fvtxdb("dead_map_run12_pp_510_sim", 367465);

  map.apply_dead_maps();

  map.dbPutAll(364574, 368798, "Dead maps based on run 367466",
      TFvtxDeadMap::BAND_ID_SIM);

}

void
UploadDeadMap_RunPeriodRun12pp()
{

  TFvtxDeadMap map;

  map.load_dead_chan_map_fvtxdb("dead_map_run12_pp_510", 1);

  map.apply_dead_maps();

  // load the dead map from fvtx database
  PHTimeStamp begin( //
      2012,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2013,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  map.dbPutAll(begin, end, "Run12 dead channel from calibration run",
      TFvtxDeadMap::BAND_ID_RUN_PERIOD);
}

void
UploadDeadMap_RunPeriodRun15()
{

  TFvtxDeadMap map;

  // remove dead wedges based on aliveness in Run15 alignment data
  // /direct/phenix+subsys+fvtx/jinhuang/miliped_work/Run15_release1_Shift_Wedge_VTX2/DST//ALL_AlignVertexDST.root_fvtx_vz0_DrawResidu_All_3-D_LateralConstraint_fvtx_vz0_fvtx_vz0.root
  map.txtGetAll(
      "/phenix/u/jinhuang/links/fvtx_data/geometry/run15_release3/NoAlignmentWedges.txt",
      "Dead wedges based on aliveness in Run15 alignment data");

  //checks
  map.Print();

  // load the dead map from fvtx database
  //  June 16 - Run 14 Au+Au at sqrt(s) = 200 GeV is over. Last physics run is with runnumber 414988.
  //  Maintenance day, switchover to 3He+Au.
  //  Jun 17 - Circulating 3He in Blue and Au in Yellow, both captured. Collision setup.
  PHTimeStamp begin( //
      2015,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2015,//int year,//
      12, //int month, //
      31, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
//  map.dbPutAll(begin, end, "dead wedges based on aliveness in Run15 alignment data",
//      TFvtxDeadMap::BAND_ID_RUN_PERIOD);
  map.dbPutAll(begin, end, "dead wedges based on aliveness in Run15 alignment data",
      TFvtxDeadMap::BAND_ID_SIM);
}


void
UploadDeadMap_RunPeriodRun14()
{

  TFvtxDeadMap map;

  // see readme in file
  map.txtGetAll(
      "/phenix/u/jinhuang/links/fvtx_data/geometry/run14_release3/NoAlignmentWedges.txt",
      "Dead wedges based on aliveness in Run14 alignment data");

  //checks
  map.Print();

  // load the dead map from fvtx database
  //  June 16 - Run 14 Au+Au at sqrt(s) = 200 GeV is over. Last physics run is with runnumber 414988.
  //  Maintenance day, switchover to 3He+Au.
  //  Jun 17 - Circulating 3He in Blue and Au in Yellow, both captured. Collision setup.
  PHTimeStamp begin( //
      2014,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2014,//int year,//
      12, //int month, //
      31, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
//  map.dbPutAll(begin, end, "dead wedges based on aliveness in Run14 alignment data",
//      TFvtxDeadMap::BAND_ID_RUN_PERIOD);
  map.dbPutAll(begin, end, "dead wedges based on aliveness in Run14 alignment data",
      TFvtxDeadMap::BAND_ID_SIM);
}


void
TextImport()
{
  gSystem->Load("libfvtxgeom.so");

  TFvtxDeadMap map;

  map.txtGetAll("dead.list", "Some comment");

  // OPTIONAL: output a copy for testing
  map.txtPutAll("dead.list_test_out");

  // time stamp for whole year 2013
  PHTimeStamp begin( //
      2013,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2014,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  // submit to the simulation band of the dead map
  // this would not affect the production dead map
  map.dbPutAll(begin, end, "Dead maps based on run13 simulation",
      TFvtxDeadMap::BAND_ID_SIM);

  // or it can be treated run by run differently
//  dead_maps.dbPutAll(first runnumber, last runnumber, "some comment");
}

void
ReadTest()
{

  recoConsts *rc = recoConsts::instance();

  TFvtxGlobalParCntrl::set_bool_par("deadmap_auto_load", false);
  TFvtxGlobalParCntrl::set_pdb_run_number(438422);

  FvtxGeom::create_arms();
  TFvtxDeadMap map;
  map.dbGetAll(438422, false);
  map.apply_dead_maps();

//
//  return;
//
//  cout << "===================================" << endl;
//
//
//  rc->set_IntFlag("RUNNUMBER", 395643);
////  TFvtxGlobalParCntrl::set_bool_par("is_sim", true);
//  TFvtxGlobalParCntrl::set_bool_par("deadmap_use_calibration_database", true);
//
//  TFvtxDeadMap::init_run();
//
//  cout << "===================================" << endl;
//
//  TFvtxDatabaseCntrl::set_pdb_run_number(367465);
//
//  TFvtxDeadMap::init_run();
//
//  cout << "===================================" << endl;
//
//  TFvtxDatabaseCntrl::set_pdb_run_number(367465);
//  TFvtxDatabaseCntrl::set_flag("deadmap_use_calibration_database", false);
//  TFvtxDatabaseCntrl::set_filename("deadmap_fvtxdb_use_custom_deadmap",
//      "dead_map_run12_pp_510_sim");
//  TFvtxDatabaseCntrl::print();
//  TFvtxDeadMap::init_run();
//
//  cout << "===================================" << endl;
//
//  TFvtxDatabaseCntrl::set_flag("deadmap_use_calibration_database", true);
//  TFvtxDatabaseCntrl::print();
//  TFvtxDeadMap::init_run();

}

void
QA_Stat(
    TString file =
        "/phenix/prod03/phnxreco/run13test/production/old0613/qaRoot-0000387551-0000.root")
{
  TFile * _file0 = new TFile(file);
  assert(_file0);

  TH3F * FVTX_hit_q_0_1 = (TH3F *) _file0->GetObjectChecked("FVTX_hit_q_0_1",
      "TH3F");
  assert(FVTX_hit_q_0_1);

  TH1F * FVTX_hit_q_0_1_stat = new TH1F("FVTX_hit_q_0_1_stat",
      "Stat for FVTX_hit_q_0_1;Log_{10}(num of hits per channel)", 100, 0,
      log10(FVTX_hit_q_0_1->GetMaximum() * 2));

  for (int x = 1; x <= FVTX_hit_q_0_1->GetNbinsX(); x++)
    for (int y = 1; y <= FVTX_hit_q_0_1->GetNbinsY(); y++)
      for (int z = 1; z <= FVTX_hit_q_0_1->GetNbinsZ(); z++)
        {
          FVTX_hit_q_0_1_stat->Fill(
              log10(FVTX_hit_q_0_1->GetBinContent(x, y, z)));
        }

  TCanvas *c1 = new TCanvas("QA_Stat", "QA_Stat", 800, 900);

  gPad->SetLogy();

  FVTX_hit_q_0_1_stat->Draw();

  SaveCanvas(c1, TString(c1->GetName()), kFALSE);
}

