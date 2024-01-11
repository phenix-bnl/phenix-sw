// $Id: FvtxCage.cxx,v 1.4 2012/02/06 05:28:03 jinhuang Exp $
/*!
  \file FvtxCage.cxx
  \brief Forward vertex Cage geometry
  \Initialize and provide access to FVTX stations
  \Initialize DCM channels
  \author Zhengyun You
  \version $Revision: 1.4 $
  \date $Date: 2012/02/06 05:28:03 $
*/

#include "TSystem.h"

#include "FvtxCage.h"
#include "FvtxStrip.h"
#include "FvtxGeom.h"
//#include "FVTXOO_FEM.h"

#include <fstream>

using namespace std;

// Feb 2012, Jin Huang <jhuang@bnl.gov>
// Redesign the mappings
//____________________________________________________________________
// this implementation reads a unique mapping for each cage and applies it to all.
//void FvtxCage::getDCMChannels( )
//{
//
//  if( !_dcm_init )
//  {
//    _dcm_init = true;
//    //cout << "FvtxCage::getDCMChannels - initialize" << endl;
//  }
//
//  int pkt_id_offset = FVTXOO_FEM::PACKET_ID_BASE;
//
//  //map file name with dummy initialization
//  const char *file= "a0_c0_map.dat";
//
//  switch( index().arm() )
//  {
//    case 0:
//    if ( index().cage() == 0 )
//    {
//      pkt_id_offset = FVTXOO_FEM::SOUTH_PACKET_ID_BASE;
//      file = "a0_c0_map.dat";
//    }
//    else
//    {
//      pkt_id_offset = FVTXOO_FEM::SOUTH_PACKET_ID_BASE
//          + FVTXOO_FEM::NPACKET_MAX_PER_CAGE;
//      file = "a0_c1_map.dat";
//    }
//    break;
//
//    case 1:
//    if ( index().cage() == 0 )
//    {
//      pkt_id_offset = FVTXOO_FEM::NORTH_PACKET_ID_BASE;
//      file = "a1_c0_map.dat";
//    }
//    else
//    {
//      pkt_id_offset = FVTXOO_FEM::NORTH_PACKET_ID_BASE + FVTXOO_FEM::NPACKET_MAX_PER_CAGE;
//      file = "a1_c1_map.dat";
//    }
//    break;
//
//    default : break;
//  }
//
//  static const int bufsize = 256;
//  char linebuf[bufsize];
//  int pkt_id, fem_id, chip_id, dcm_channel, station, wedge, column, strip;
//
//  std::string file_name = file;
//  bool local_file = gSystem->AccessPathName(file_name.c_str());
//  if (local_file)
//  {
//    // local file is not available, try to find file in public area
//    file_name = FvtxGeom::get_public_file_path();
//    file_name.append( file );
//    bool public_file = gSystem->AccessPathName(file_name.c_str());
//    if (public_file)
//    {
//      // file not found in public area, exit
//       std::ostringstream s;
//       s << "Failed to open file " << file_name;
//       std::cout << "FvtxCage::getDCMChannels: WARNING: " << s.str()
//        << ", bailing out ..." << std::endl;
//       std::cout << "Please locate this file and try again." << std::endl;
//       exit(1);
//    }
//  }
//
//  ifstream s(file_name.c_str());
//  if (!s){
//    cout << "Error opening file " << file_name <<". \n";
//    s.close();
//    return;
//  }
//  else
//    cout << "FvtxCage::getDCMChannels - Arm"<<index().arm()
//    <<" Cage"<<index().cage()<<": use Configuration File "<<file_name<<endl;
//
//  while(s.getline(linebuf,bufsize,'\n'))
//  {
//
//    istringstream stringbuf(linebuf);
//
//    stringbuf >> pkt_id >> fem_id >> chip_id >> dcm_channel >> station >> wedge >> column >> strip;
//
//    if ( station >= 0 && station < FVTXGEOM::NumberOfStations &&
//         wedge >= 0 && wedge < FVTXGEOM::NumberOfSectors &&
//         column >= 0 && column < FVTXGEOM::NumberOfColumns )
//    {
//      FvtxStrip *p_strip = get_station( station )->get_sector( wedge )->get_column( column )->get_strip( strip );
//      if ( !p_strip) cerr << "FvtxCage::getDCMChannel(), strip [" << station << ", " << wedge << ", " << column << ", " << strip << "] not found " << endl;
//
//      p_strip->set_packet_id( pkt_id_offset + pkt_id );
//      p_strip->set_fem_id( fem_id );
//      p_strip->set_chip_id( chip_id );
//      p_strip->set_dcm_channel( dcm_channel );
//    }
//  }
//  s.close();
//}

