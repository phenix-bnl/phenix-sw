// $Id: FvtxGlobalPar_Database.C,v 1.9 2016/03/10 19:09:26 jinhuang Exp $                                                                                             

/*!
 * \file FvtxGlobalPar_Database.C
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.9 $
 * \date $Date: 2016/03/10 19:09:26 $
 */

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
FvtxGlobalPar_Database()
{
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libfvtxgeom.so");

//  PdbCalParameters_Test();

//  SaveRun14AuAu();
//  SaveDefault();

//  SaveRun12pp510();
//  SaveRun15pp();

//  SaveRun15pAu();
//  SaveRun15pAl();

//  SaveRun15pp_Offline_withAvgBeamPos();
//  SaveRun15pp_Offline_Pro105()();
  SaveRun15pAu_Offline_Pro105();

//  ReadTest_Test();
//  SaveRun16AuAu();
}

void
PdbCalParameters_Test()
{
  TFvtxGlobalParCntrl::test();
}

void
ReadTest_Test()
{
  gSystem->Load("libfvtxgeom.so");
  TFvtxGlobalParCntrl::set_pdb_run_number(433031);
  TFvtxGlobalParCntrl::init_run();
  TFvtxGlobalParCntrl::print();
}

void
SaveRun14AuAu()
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", false);

  const double beamcenter_x = 0.3532;
  const double beamcenter_y = 0.0528;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

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
      6, //int month, //
      17, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "Heavy ion reco flags for AuAu production");

}

void
SaveRun15pp()
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", false);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

  const double beamcenter_x = 0.15;
  const double beamcenter_y = 0.08;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

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

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "Initial parameters for Run15 200 GeV pp online production");

}




void
SaveRun15pp_Offline()
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

  const double beamcenter_x = 0.15;
  const double beamcenter_y = 0.08;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

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
      5, //int month, //
      4, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "Final parameters for Run15 200 GeV pp production");

}

//! Update Run15 global parameter with average beam position
void
SaveRun15pp_Offline_withAvgBeamPos(
    string beam_pos_file =
        "/direct/phenix+subsys+fvtx/jinhuang/taxi/Run15pp200MuonsMUPro104/6729/Pass2/se-ALL.lst.root.BeamPosTest" //
    )
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

  const double beamcenter_x = 0.15;
  const double beamcenter_y = 0.08;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

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
      5, //int month, //
      4, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  cout << "Reading " << beam_pos_file << endl;
  fstream fin;
  fin.open(beam_pos_file.c_str(), ios_base::in);

  int count = 0;

  string line;
  while (!fin.eof())
    {
      getline(fin, line);

      if (line.length() > 0)
        {
          int run = 0;
          float beamx = 0;
          float beamy = 0;

          stringstream s(line);

          s >> run >> beamx >> beamy;
          cout << "Run " << run << " <x,y> = " << beamx << "," << beamy << endl;

          if (run > 0)
            {
              TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamx);
              TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamy);
              TFvtxGlobalParCntrl::set_bool_par("beam_use_average_xy", true);

              TFvtxGlobalParCntrl::set_pdb_run_number(run);

              TFvtxGlobalParCntrl::print();

              stringstream s2;
              s2
                  << "pp-collision flags using VTX and average beam position (calibrated with taxi 6729) for run "
                  << run;
              TFvtxGlobalParCntrl::save_pdb_parameter(run, run,
                  s2.str().c_str());

              count++;
            }

        }

    }

  cout << "Done: uploading " << count << " run-by-run beam centerings from "
      << beam_pos_file << endl;
}

//! Update Run15 global parameter with average beam position for Pro.105 production
void
SaveRun15pp_Offline_Pro105(
    string beam_pos_file =
        "/phenix/u/jinhuang/links/fvtx_data/taxi/Run15pp200MuonsMUPro105/8065_Iter1/dca_hist.root.lst.root.BeamPosTest" //
    )
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

  const double beamcenter_x = 0.15;
  const double beamcenter_y = 0.08;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

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
      5, //int month, //
      4, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  cout << "Reading " << beam_pos_file << endl;
  fstream fin;
  fin.open(beam_pos_file.c_str(), ios_base::in);

  int count = 0;

  string line;
  while (!fin.eof())
    {
      getline(fin, line);

      if (line.length() > 0)
        {
          int run = 0;
          float beamx = 0;
          float beamy = 0;
          float beamdx = 0;
          float beamdy = 0;
          float tmp = 0;

          stringstream s(line);

          s >> run >> beamx >> beamy;
          s >> tmp >> tmp; // eror x, y
          s >> beamdx >> beamdy; // eror x, y
          cout << "Run " << run << " <x,y> = " << beamx << "," << beamy
              << " <dx,dy>/dz = " << beamdx << "," << beamdy << endl;

          if (run > 0)
            {
              TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamx);
              TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamy);
              TFvtxGlobalParCntrl::set_float_par("beam_dxdz", beamdx);
              TFvtxGlobalParCntrl::set_float_par("beam_dydz", beamdy);
              TFvtxGlobalParCntrl::set_bool_par("beam_use_average_xy", true);

              TFvtxGlobalParCntrl::set_pdb_run_number(run);

              TFvtxGlobalParCntrl::print();

              stringstream s2;
              s2
                  << "pp-collision flags using VTX and average beam position (calibrated with Pro.105 taxi 8065) for run "
                  << run;
              TFvtxGlobalParCntrl::save_pdb_parameter(run, run,
                  s2.str().c_str());

              count++;
            }

        }

    }

  cout << "Done: uploading " << count << " run-by-run beam centerings from "
      << beam_pos_file << endl;
}


//! Update Run15 global parameter with average beam position for Pro.105 production
void
SaveRun15pAu_Offline_Pro105(
    string beam_pos_file =
        "/phenix/u/jinhuang/links/fvtx_data/geometry/run15_release3/Run15pAu200MuonsMBPro105.BeamPos" //
    )
{
//
  //
    TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
    TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

  //  435000-436000
  //  EXT PARAMETER                                   STEP         FIRST
  //   NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE
  //    1  Constant     2.79510e+04   8.16009e+01   1.76094e+00   1.09032e-05
  //    2  Mean         2.09338e-01   1.65858e-04   4.98014e-06   1.42198e+00
  //    3  Sigma        7.17272e-02   1.79167e-04   1.47427e-05   6.65222e-01

    const double beamcenter_x = 2.09338e-01;

  //  EXT PARAMETER                                   STEP         FIRST
  //  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE
  //   1  Constant     3.03288e+04   1.03809e+02   5.00810e-01  -1.39746e-05
  //   2  Mean         5.37200e-02   2.30364e-04   1.30582e-06  -8.59097e-01
  //   3  Sigma        5.16267e-02   3.20981e-04   6.10443e-06  -1.97996e-01

    const double beamcenter_y = 5.37200e-02;

    TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
    TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

    TFvtxGlobalParCntrl::print();

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
      5, //int month, //
      4, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  cout << "Reading " << beam_pos_file << endl;
  fstream fin;
  fin.open(beam_pos_file.c_str(), ios_base::in);

  int count = 0;

  string line;
  while (!fin.eof())
    {
      getline(fin, line);

      if (line.length() > 0)
        {
          int run = 0;
          float beamx = 0;
          float beamy = 0;
          float beamdx = 0;
          float beamdy = 0;
          float tmp = 0;

          stringstream s(line);

          s >> run >> beamx >> beamy;
          s >> tmp >> tmp; // eror x, y
          s >> beamdx >> beamdy; // eror x, y
          cout << "Run " << run << " <x,y> = " << beamx << "," << beamy
              << " <dx,dy>/dz = " << beamdx << "," << beamdy << endl;

          if (run > 0)
            {
              TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamx);
              TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamy);
              TFvtxGlobalParCntrl::set_float_par("beam_dxdz", beamdx);
              TFvtxGlobalParCntrl::set_float_par("beam_dydz", beamdy);
              TFvtxGlobalParCntrl::set_bool_par("beam_use_average_xy", true);

              TFvtxGlobalParCntrl::set_pdb_run_number(run);

              TFvtxGlobalParCntrl::print();

              stringstream s2;
              s2
                  << "pAu-collision flags using VTX and average beam position (calibrated with Run15pAu200MuonsMBPro105 taxi 8326) for run "
                  << run;
              TFvtxGlobalParCntrl::save_pdb_parameter(run, run,
                  s2.str().c_str());

              count++;
            }

        }

    }

  cout << "Done: uploading " << count << " run-by-run beam centerings from "
      << beam_pos_file << endl;
}


void
SaveRun15pAu()
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

//  435000-436000
//  EXT PARAMETER                                   STEP         FIRST
//   NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE
//    1  Constant     2.79510e+04   8.16009e+01   1.76094e+00   1.09032e-05
//    2  Mean         2.09338e-01   1.65858e-04   4.98014e-06   1.42198e+00
//    3  Sigma        7.17272e-02   1.79167e-04   1.47427e-05   6.65222e-01

  const double beamcenter_x = 2.09338e-01;

//  EXT PARAMETER                                   STEP         FIRST
//  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE
//   1  Constant     3.03288e+04   1.03809e+02   5.00810e-01  -1.39746e-05
//   2  Mean         5.37200e-02   2.30364e-04   1.30582e-06  -8.59097e-01
//   3  Sigma        5.16267e-02   3.20981e-04   6.10443e-06  -1.97996e-01

  const double beamcenter_y = 5.37200e-02;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

//  2015-05-04 00:57:41
  PHTimeStamp begin( //
      2015,//int year,//
      5, //int month, //
      4, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
//  2015-06-08 09:31:03
  PHTimeStamp end( //
      2015,//int year,//
      6, //int month, //
      9, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "parameters for Run15 200 GeV pAu production");

}

void
SaveRun15pAl()
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", false);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

//  root [2] T->Add("run_0000438000_0000439000/smpDST/smpDST_*-0000.root")
//  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE
//   1  Constant     1.53769e+04   6.05372e+01   4.36864e-01  -5.38755e-06
//   2  Mean         2.07118e-01   2.39924e-04   2.37669e-06   8.82923e-02
//   3  Sigma        6.69816e-02   2.87314e-04   1.22119e-05   2.62084e-02

  const double beamcenter_x = 2.07118e-01;

//  RATE
//    EXT PARAMETER                                   STEP         FIRST
//    NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE
//     1  Constant     1.77631e+04   7.11913e+01   5.19931e-01  -4.33656e-06
//     2  Mean         6.91379e-02   2.22763e-04   2.26723e-06  -9.16446e-02
//     3  Sigma        6.15952e-02   2.72063e-04   1.22233e-05   6.84441e-02

  const double beamcenter_y = 6.91379e-02;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

//  2015-06-09 03:46:20
  PHTimeStamp begin( //
      2015,//int year,//
      6, //int month, //
      9, //int day, //
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

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "parameters for Run15 200 GeV pAl production");

}



void
SaveRun16AuAu()
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", false);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", false);

  const double beamcenter_x = 0.258661;
  const double beamcenter_y = 0.0351448;

  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

  PHTimeStamp begin( //
      2016,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2016,//int year,//
      12, //int month, //
      31, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "Initial parameters for Run16 200 GeV AuAu online production");

}

void
SaveRun12pp510(
    string beam_pos_file =
//        "/phenix/u/jinhuang/links/fvtx_data/geometry/release10/MWG_MU.lst_vtx_hist.root.BeamPos"//
        "/direct/phenix+subsys+fvtx/jinhuang/taxi/Run12pp510MuonPro100/4117/se-ALL.lst.root.BeamPos"//
    )
{
//
  TFvtxGlobalParCntrl::set_bool_par("use_svx", true);
  TFvtxGlobalParCntrl::set_bool_par("is_pp", true);

//  const double beamcenter_x = 0.3532;
//  const double beamcenter_y = 0.0528;
//
//  TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamcenter_x);
//  TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamcenter_y);

  TFvtxGlobalParCntrl::print();

  PHTimeStamp begin( //
      2012,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

//  Apr 18 .......... Run-12 510 GeV pp run ended at 12:30pm on April 18. PHENIX goal of the samples luminosity was reached.
  PHTimeStamp end( //
      2012,//int year,//
      4, //int month, //
      19, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "Basic pp-collision flags using VTX");

  cout << "Run-by-run beam centering from " << beam_pos_file << endl;

  fstream fin;
  fin.open(beam_pos_file.c_str(), ios_base::in);

  int count = 0;

  string line;
  while (!fin.eof())
    {
      getline(fin, line);

      if (line.length() > 0)
        {
          int run = 0;
          float beamx = 0;
          float beamy = 0;

          stringstream s(line);

          s >> run >> beamx >> beamy;

          if (run > 0)
            {
              TFvtxGlobalParCntrl::set_float_par("beam_x_seed", beamx);
              TFvtxGlobalParCntrl::set_float_par("beam_y_seed", beamy);
              TFvtxGlobalParCntrl::set_bool_par("beam_use_average_xy", true);

              TFvtxGlobalParCntrl::set_pdb_run_number(run);

              TFvtxGlobalParCntrl::print();

              stringstream s2;
              s2
                  << "pp-collision flags using VTX and average beam position (calibrated with taxi 4117) for run "
                  << run;
              TFvtxGlobalParCntrl::save_pdb_parameter(run, run,
                  s2.str().c_str());

              count++;
            }

        }

    }

  cout << "Done: uploading " << count << " run-by-run beam centerings from "
      << beam_pos_file << endl;

}

void
SaveDefault()
{
  TFvtxGlobalParCntrl::print();

  PHTimeStamp begin( //
      1914,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2114,//int year,//
      6, //int month, //
      17, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  TFvtxGlobalParCntrl::save_pdb_parameter(begin, end,
      "Default parameters as place holder");

}
