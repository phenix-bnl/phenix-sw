// $Id: FvtxGeom_Test.C,v 1.7 2021/05/19 18:10:13 jinhuang Exp $                                                                                             

/*!
 * \file FvtxGeom_Test.C
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.7 $
 * \date $Date: 2021/05/19 18:10:13 $
 */

void
FvtxGeom_Test()
{
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libfvtxgeom.so");

  RunToTimePg::Register();

//  SaveDefaultGeom();

//  SaveRun12Geom_Prod();
//    SaveRun13Geom_Prod();
//  SaveRun12Geom_Sim();

  ReadTest();

}

void
ReadTest()
{
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER", 387292);
//  TFvtxDatabaseCntrl::set_pdb_run_number(387292);
  TFvtxDatabaseCntrl::set_flag("is_sim", false);
//  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", true);

  TFvtxDatabaseCntrl::print();
  FvtxGeom::create_arms();
  const double z = FvtxGeom::get_arm(1)->get_cage(0)->get_station(0)->get_z();
  cout << "station0 z = " << z << endl;
}

void
SaveRun12Geom_Prod()
{
//
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);
  TFvtxDatabaseCntrl::set_filename("geom_root_file_name",
      "fvtxgeom_release3.root");
  FvtxGeom::get_arm(0);

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

  FvtxGeom::save_pdb_geometry(begin, end,
      "release3 geometry for run12 production");
}

void
SaveRun13Geom_Prod()
{
//
  TFvtxDatabaseCntrl::set_flag("geom_use_calibration_database", false);
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path",
      "/direct/phenix+subsys+fvtx/jinhuang/miliped_work/Run13_Station_Iter1_Wedge_Iter2/");
  TFvtxDatabaseCntrl::set_filename("geom_root_file_name",
      "Fvtxgeom_new_run13_release1.root");
  FvtxGeom::get_arm(0);

  // load the dead map from fvtx database
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

  FvtxGeom::save_pdb_geometry(begin, end,
      "release 1 geometry for run13 production");
}

void
SaveRun12Geom_Sim()
{

  TFvtxDatabaseCntrl::set_filename("geom_root_file_name",
      "geom_with_survey_v4.root");
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path",
      "/phenix/u/jinhuang/work/FVTX/Fun4FVTX/millipede/");
  FvtxGeom::get_arm(0);

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

  FvtxGeom::save_pdb_geometry(begin, end,
      "Ideal geometry w/cage survey for run12 simulation",
      FvtxGeom::BANK_ID_SIM);
}

void
SaveDefaultGeom()
{

  TFvtxDatabaseCntrl::set_filename("geom_root_file_name",
      "geom_with_survey_v4.root");
  TFvtxDatabaseCntrl::set_filename("geom_root_file_path",
      "/phenix/u/jinhuang/work/FVTX/Fun4FVTX/millipede/");
  FvtxGeom::get_arm(0);

  // load the dead map from fvtx database
  PHTimeStamp begin( //
      2001,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );
  PHTimeStamp end( //
      2021,//int year,//
      1, //int month, //
      1, //int day, //
      0, //int hour, //
      0, //int minute, //
      0, //int second, //
      0 //int fraction//
      );

  FvtxGeom::save_pdb_geometry(begin, end,
      "Run12 ideal geometry for default option", FvtxGeom::BANK_ID_SIM);
  FvtxGeom::save_pdb_geometry(begin, end,
      "Run12 ideal geometry for default option", FvtxGeom::BANK_ID_RUN);
}

