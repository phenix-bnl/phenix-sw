// $Id: GeometryDump.C,v 1.1 2021/05/19 18:37:02 jinhuang Exp $

/*!
 * \file GeometryCheck.C
 * \brief
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2021/05/19 18:37:02 $
 */

#include <TSystem.h>

#include <iostream>

using namespace std;

void GeometryDump(const char* root_geometry_file = "fvtxgeom_run16_release1.root") {
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libfvtxgeom.so");
  //
  TFvtxGlobalParCntrl::set_pdb_run_number(443252);

  TFvtxGlobalParCntrl::set_bool_par("geom_use_calibration_database", false);
  //

  TFvtxGlobalParCntrl::set_string_par("geom_root_file_path", "./");
  TFvtxGlobalParCntrl::set_string_par("geom_root_file_name",
                                      root_geometry_file);

  FvtxGeom::create_arms();

  std::cout.setf(std::ios::fixed, std::ios::floatfield);
  std::cout.precision(3);
  //  std::cout.precision(6);

  for (unsigned int arm_id = 0; arm_id < FVTXGEOM::NumberOfArms; arm_id++) {
    FvtxArm* arm = FvtxGeom::get_arm(arm_id);

    cout << "arm " << arm_id << endl;
    //      cout << "|-- get_z \t=" << arm->get_z() << endl;

    for (unsigned int cage_id = 0; cage_id < FVTXGEOM::NumberOfCages;
         cage_id++) {
      FvtxCage* cage = arm->get_cage(cage_id);

      cout << "cage " << arm_id << " - " << cage_id << endl;
      cout << "|-- get_z \t=" << cage->get_z() << endl;
      cout << "|-- get_R \t=" << cage->get_r() << endl;
      cout << "|-- get_phi \t=" << cage->get_phi() << endl;
      //          cout << "|-- get_phi0 \t=" << cage->get_phi0() << endl;

      for (unsigned int station_id = 0; station_id < FVTXGEOM::NumberOfStations;
           station_id++) {
        FvtxStation* station = cage->get_station(station_id);

        cout << "station " << arm_id << " - " << cage_id << " - " << station_id
             << endl;
        cout << "|-- get_x \t=" << station->get_center().getX() << endl;
        cout << "|-- get_y \t=" << station->get_center().getY() << endl;
        cout << "|-- get_z \t=" << station->get_z() << endl;
        cout << "|-- get_R \t=" << station->get_r() << endl;
        cout << "|-- get_phi \t=" << station->get_phi() << endl;
        //              cout << "|-- get_phi_begin \t=" <<
        //              station->get_phi_begin() << endl; cout << "|--
        //              get_phi_end \t=" << station->get_phi_end() << endl;
        cout << "|-- get_inner_radius \t=" << station->get_inner_radius()
             << endl;
        cout << "|-- get_inner_radius \t=" << station->get_inner_radius()
             << endl;

        for (unsigned int sector_id = 0; sector_id < FVTXGEOM::NumberOfSectors;
             sector_id++) {
          FvtxSector* sector = station->get_sector(sector_id);

          cout << "sector " << arm_id << " - " << cage_id << " - " << station_id
               << " - " << sector_id << endl;
          cout << "|-- get_x \t=" << sector->get_center().getX() << endl;
          cout << "|-- get_y \t=" << sector->get_center().getY() << endl;
          cout << "|-- get_z \t=" << sector->get_z() << endl;
          cout << "|-- get_R \t=" << sector->get_r() << endl;
          cout << "|-- get_phi \t=" << sector->get_phi() << endl;
          cout << "|-- get_inner_radius \t=" << sector->get_inner_radius()
               << endl;
          cout << "|-- get_inner_radius \t=" << sector->get_inner_radius()
               << endl;
          cout << "|-- get_delta_z \t=" << sector->get_delta_z() << endl;
          cout << "|-- get_phi_begin \t=" << sector->get_phi_begin() << endl;
          cout << "|-- get_phi_end \t=" << sector->get_phi_end() << endl;

          for (unsigned int column_id = 0;
               column_id < FVTXGEOM::NumberOfColumns; column_id++) {
            FvtxColumn* column = sector->get_column(column_id);

            cout << "column " << arm_id << " - " << cage_id << " - "
                 << station_id << " - " << sector_id << " - " << column_id
                 << endl;
            cout << "|-- get_z \t=" << column->get_z() << endl;
            cout << "|-- get_R \t=" << column->get_r() << endl;
            cout << "|-- get_phi \t=" << column->get_phi() << endl;
            cout << "|-- get_inner_radius \t=" << column->get_inner_radius()
                 << endl;
            cout << "|-- get_inner_radius \t=" << column->get_inner_radius()
                 << endl;
            cout << "|-- get_delta_z \t=" << column->get_delta_z() << endl;
            cout << "|-- get_phi_begin \t=" << column->get_phi_begin() << endl;
            cout << "|-- get_phi_end \t=" << column->get_phi_end() << endl;
            cout << "|-- get_n_strips \t=" << column->get_n_strips() << endl;
            cout << "|-- get_strip_tilt \t=" << column->get_strip_tilt()
                 << endl;

            //                      for (unsigned int strip_id = 0;
            //                          strip_id < column->get_n_strips();
            //                          strip_id++)
            //                        {
            //                          // create strip
            //                          FvtxStrip* strip =
            //                          column->get_strip(strip_id); cout <<
            //                          "column " << arm_id << " - " << cage_id
            //                              << " - " << station_id << " - " <<
            //                              sector_id
            //                              << " - " << column_id << endl;
            //                          cout << "|-- get_strip_index \t="
            //                              << strip->get_strip_index() << endl;
            //                          cout << "|-- get_width \t=" <<
            //                          strip->get_width()
            //                              << endl;
            //                          cout << "|-- get_thickness \t="
            //                              << strip->get_thickness() << endl;
            //                          cout << "|-- get_position_begin \t="
            //                              <<
            //                              strip->get_position_begin().getX()
            //                              << ", "
            //                              <<
            //                              strip->get_position_begin().getY()
            //                              << ", "
            //                              <<
            //                              strip->get_position_begin().getZ()
            //                              << endl;
            //                          cout << "|-- get_position_end \t="
            //                              << strip->get_position_end().getX()
            //                              << ", "
            //                              << strip->get_position_end().getY()
            //                              << ", "
            //                              << strip->get_position_end().getZ()
            //                              << endl;
            //                        }
          }
        }
      }
    }
  }
}
