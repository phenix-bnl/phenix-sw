//#include "FvtxArm.h"

void root_geom_test()
{
  std::cout << "begin of geometry test" << std::endl;

  gSystem->Load("libfun4all.so");
  gSystem->Load("/direct/phenix+spin/phnxsp01/youzy/fvtx/source/fvtx_align/install/lib/libfvtxgeom.so");
  //gSystem->Load("libfvtxgeom.so");

  //FVTXGEOM::set_config(1);
  //FvtxGeom::set_verbosity( FVTXGEOM::ALOT );
  FvtxGeom::create_arms();
  //FvtxGeom::get_phy_sector(0,0,0)->Print();
  //FvtxGeom::get_phy_radius(0,0,0,0,0)->Print();

  //FvtxArm *arm = FvtxGeom::south_arm();
  //arm->print();
 
  bool print_SISI   = 0; 
  bool test_station = 1;
  bool test_sector  = 1;
  bool test_column  = 0;
  bool test_strip   = 0;

  if  (print_SISI) {
    for (int arm_id = 0; arm_id < 2; arm_id++) {
      for (int cage_id = 0; cage_id < 4; cage_id++) {
        for (int station_id = 0; station_id < 4; station_id++) {
          for (int sector_id = 0; sector_id < 24; sector_id++) {
	    std::cout << FvtxGeom::get_phy_sector(arm_id, cage_id, station_id, sector_id)->GetName() << std::endl;
	    std::cout << ((TGeoTrd1*)FvtxGeom::get_phy_sector(arm_id, cage_id, station_id, sector_id)->GetShape())->GetDz() << std::endl;
	  }
	}
      }
    } 
    std::cout << std::endl;
  }

  if (test_station) { 
    std::cout << std::endl << "Station info... " << std::endl << std::endl;

    for (int arm_id = 0; arm_id < 2; arm_id++) {
      FvtxArm *arm = FvtxGeom::get_arm(arm_id);
      for (int cage_id = 0; cage_id < 2; cage_id++) {
        FvtxCage *cage = arm->get_cage(cage_id);
        for (int station_id = 0; station_id < 4; station_id++) {
          FvtxStation *station = cage->get_station(station_id);
        //station->get_phy_node()->GetVolume()->InspectShape();
        //std::cout << station->get_phy_node()->GetShape()->GetName() << std::endl;
        //std::cout << ((TGeoPcon*)station->get_phy_node()->GetShape())->GetRmin(0) << std::endl;
          std::cout << " arm "  << arm_id
                    << " cage " << cage_id
	            << " station " << station_id
	            << " z "    << station->get_z()
                    << " rmin " << station->get_inner_radius()
                    << " rmax " << station->get_outer_radius()
		    << " phi0 " << station->get_phi0()
		    << std::endl;
        }
      }
    }
    std::cout << std::endl;
  }

  if (test_sector) {
    std::cout << std::endl << "Sector info... " << std::endl << std::endl;

    for (int arm_id = 0; arm_id < 2; arm_id++) {
      FvtxArm *arm = FvtxGeom::get_arm(arm_id);
      for (int cage_id = 0; cage_id < 2; cage_id++) {
        FvtxCage *cage = arm->get_cage(cage_id);
        for (int station_id = 0; station_id < 4; station_id++) {
          FvtxStation *station = cage->get_station(station_id);
	  for (int sector_id = 0; sector_id < 24; sector_id++) {
	    FvtxSector *sector = station->get_sector(sector_id);
            //std::cout << sector->get_phy_node()->GetShape()->GetName() << std::endl;
    	    std::cout << " arm "      << arm_id
                      << " cage " << cage_id
	  	      << " station "  << station_id
		      << " sector "   << sector_id
		      << " z "        << sector->get_z()
		      << " dz "       << sector->get_delta_z()
		      << " rmin "     << sector->get_inner_radius()
		      << " rmax "     << sector->get_outer_radius()
		      << " overlap "  << sector->get_overlap()
		      << " phibegin " << sector->get_phi_begin()
		      << " phiend "   << sector->get_phi_end()
		      << std::endl;
          }
	}
      }
    }
    std::cout << std::endl;
  }

  if (test_column) {
    std::cout << std::endl << "Column info... " << std::endl << std::endl;

    for (int arm_id = 0; arm_id < 2; arm_id++) {
      FvtxArm *arm = FvtxGeom::get_arm(arm_id);
      for (int cage_id = 0; cage_id < 2; cage_id++) {
        FvtxCage *cage = arm->get_cage(cage_id);
        for (int station_id = 0; station_id < 4; station_id++) {
          FvtxStation *station = cage->get_station(station_id);
          for (int sector_id = 0; sector_id < 24; sector_id++) {
            FvtxSector *sector = station->get_sector(sector_id);
            for (int column_id = 0; column_id < 2; column_id++) {
              FvtxColumn *column = sector->get_column(column_id);

              std::cout << " arm "      << arm_id
                        << " cage " << cage_id
                        << " station "  << station_id
                        << " sector "   << sector_id
                        << " column "   << column_id
                        << " z "        << column->get_z()
                        << " rmin "     << column->get_inner_radius()
                        << " rmax "     << column->get_outer_radius()
                        << " overlap "  << column->get_overlap()
                        << " phibegin " << column->get_phi_begin()
                        << " phiend "   << column->get_phi_end()
                        << std::endl;
            }
          }
        }
      }
    }
    std::cout << std::endl;
  }


  if (test_strip) {
    std::cout << std::endl << "Strip info... " << std::endl << std::endl;

    for (int arm_id = 0; arm_id < 1; arm_id++) {
      FvtxArm *arm = FvtxGeom::get_arm(arm_id);
      for (int cage_id = 0; cage_id < 2; cage_id++) {
        FvtxCage *cage = arm->get_cage(cage_id);
        for (int station_id = 0; station_id < 2; station_id++) {
          FvtxStation *station = cage->get_station(station_id);
          for (int sector_id = 0; sector_id < 1; sector_id++) {
            FvtxSector *sector = station->get_sector(sector_id);
            for (int column_id = 0; column_id < 2; column_id++) {
              FvtxColumn *column = sector->get_column(column_id);
              for (int strip_id = 0; strip_id < column->get_n_strips(); strip_id++) {
                FvtxStrip *strip = column->get_strip(strip_id);

                std::cout << " arm "      << arm_id
                          << " cage "     << cage_id
                          << " station "  << station_id
                          << " sector "   << sector_id
                          << " column "   << column_id
                          << " strip "    << strip_id
                          << " z "        << strip->get_z()
                          << " width "    << strip->get_width()
                          << " thick "    << strip->get_thickness()
                          << " r "        << strip->get_r()
                          << " phi "      << strip->get_phi()
                          << " center ( " << strip->get_center().getX() << ", " << strip->get_center().getY() << ", " << strip->get_center().getZ() << ") "
                            //<< " posbegin (" << strip->get_position_begin().getX() << ", " << strip->get_position_begin().getY() << ", " << strip->get_position_begin().getZ() << ") "
                            //<< " posend (" << strip->get_position_end().getX() << ", " << strip->get_position_end().getY() << ", " << strip->get_position_end().getZ() << ") "
                            << std::endl;
              }
            }
          }
        }
      }
    }
    std::cout << std::endl;
  }

  std::cout << "end of geometry test" << std::endl;
}
