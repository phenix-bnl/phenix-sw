// $Id: ConvertTextToGeometry.C,v 1.1 2021/05/19 18:59:20 jinhuang Exp $                                                                                             
 
/*!
 * \file ConvertTextToGeometry.C
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2021/05/19 18:59:20 $
 */


void
ConvertTextToGeometry()
{

  gSystem->Load("libfvtx_subsysreco.so");
  gSystem->Load("libfvtxgeom.so");


  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  TFvtxGlobalParCntrl::set_pdb_run_number(443252);

  TFvtxGlobalParCntrl::set_bool_par("deadmap_auto_load", true);
  TFvtxGlobalParCntrl::set_bool_par("geom_use_calibration_database", false);
  TFvtxGlobalParCntrl::set_string_par("geom_root_file_path", "./");

  TFvtxGlobalParCntrl::set_string_par("geom_root_file_name",
      "geom_raw_hex_rotations.root");

  se->registerSubsystem(new FvtxDisalign("FVTXDISALIGN", "aligment.txt"));
  FvtxGeom::save_root_geometry("Fvtxgeom_new.root");

}

