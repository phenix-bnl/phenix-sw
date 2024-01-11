//#include "FvtxArm.h"

void make_dcm_channel_map()
{
  std::cout << "begin of make_dcm_channel_map" << std::endl;

  gSystem->Load("libfun4all.so");
  //gSystem->Load("/direct/phenix+spin/phnxsp01/youzy/fvtx/source/cvs_test2/install/lib/libfvtxgeom.so");
  gSystem->Load("libfvtxgeom.so");

  //FvtxGeom::set_verbosity( FVTXGEOM::ALOT );
  FvtxGeom::create_arms();
  //FvtxGeom::get_phy_sector(0,0,0)->Print();
  FvtxGeom::get_phy_column(0,0,0,0,0)->Print();

  //FvtxArm *arm = FvtxGeom::south_arm();
  //arm->print();

  if (1) {
    std::cout << std::endl << "Strip info... " << std::endl << std::endl;

    int pkt_id, fem_id, chip_id, dcm_channel;
    int channel;

    for (int arm_id = 0; arm_id < 2; arm_id++) {
      FvtxArm *arm = FvtxGeom::get_arm(arm_id);
      for (int cage_id = 0; cage_id < 2; cage_id++) {
        FvtxCage *cage = arm->get_cage(cage_id);

        char file_name[512];
        sprintf(file_name, "a%i_c%i_map.dat", arm_id, cage_id);
        //TString cmd("rm -f ");
        //cmd += file_name;
        //gSystem->Exec(cmd);

        ofstream s(file_name, ios_base::trunc);

        for (int station_id = 0; station_id < 4; station_id++) {
          FvtxStation *station = cage->get_station(station_id);
          for (int sector_id = 0; sector_id < 24; sector_id++) {
            FvtxSector *sector = station->get_sector(sector_id);
            pkt_id = sector_id/2;
            fem_id = (station_id/2)*2 + sector_id%2 + 1;

            for (int column_id = 0; column_id < 2; column_id++) {
              FvtxColumn *column = sector->get_column(column_id);
              for (int strip_id = 0; strip_id < column->get_n_strips(); strip_id++) {
                FvtxStrip *strip = column->get_strip(strip_id);

                channel = column->get_n_strips() * column_id + strip_id;
                chip_id = channel/128 + 1;
                if (station_id == 1) chip_id += 10;
                else if (station_id == 3) chip_id += 26;
                dcm_channel = channel % 128;

                s << pkt_id << "\t" << fem_id << "\t" << chip_id << "\t" << dcm_channel 
                  << "\t" << station_id << "\t" << sector_id << "\t" << column_id << "\t" << strip_id << endl;

                std::cout << " arm "      << arm_id
                          << " cage "     << cage_id
                          << " station "  << station_id
                          << " sector "   << sector_id
                          << " column "   << column_id
                          << " strip "    << strip_id
                          << std::endl;
              }
            }
          }
        }

        s.close();
      }
    }
    std::cout << std::endl;
  }

  std::cout << "end of make dcm channel map" << std::endl;
}
