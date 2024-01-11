// $Id: FvtxDCMChannelMap.cxx,v 1.7 2014/01/26 15:40:20 bbannier Exp $

/*!
 * \file FvtxDCMChannelMap.cxx
 * \brief DAQ address -> FvtxStrip mapping
 * \version $Revision: 1.7 $
 * \date $Date: 2014/01/26 15:40:20 $
 */



#include <map>
#include <sstream>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <sstream>
#include <stdio.h>

#include "FvtxArm.h"
#include "FvtxDCMChannelMap.h"
#include "FvtxStrip.h"
#include "FVTXOO_FEM.h"
#include "FvtxGeom.h"

using namespace std;

//_______________________________________________________________________
string
FvtxDCMChannelMap::bankName(int arm, int cage, string tag)
{

  // create filename if none is specified
  ostringstream what;
  what << "fvtx.";
  switch (cage)
    {
  case 0:
    what << "Cage0ChannelMap";
    break;

  case 1:
    what << "Cage1ChannelMap";
    break;

  default:
    cout << "FvtxDCMChannelMap::bankName - unrecognized cage id: " << cage
        << endl;
    break;
    }

  what << "." << (arm == FVTXGEOM::South ? "South" : "North");

  if (!tag.empty())
    what << "." << tag;
  return what.str();

}

//_______________________________________________________________________
FvtxDCMChannelMap::FvtxDCMChannelMap() :
    _verbosity(FVTXGEOM::NONE), error_count(0)
{
  for (int i = 0; i < FVTXOO_FEM::LINEAR_CHAN_MAP::MAX_CHAN; i++)
    _map[i] = NULL;

  MapStrips();
}

//_______________________________________________________________________
FvtxStrip*
FvtxDCMChannelMap::getFvtxStrip(int packet_id, int fem_id, int chip_id,
    int dcm_channel)
{

  int linear_chan = FVTXOO_FEM::LINEAR_CHAN_MAP::GET_CHAN(
  /*const int*/packet_id,
  /*const int*/fem_id,
  /*const int*/chip_id,
  /*const int*/dcm_channel);

  if (linear_chan < 0 || linear_chan > FVTXOO_FEM::LINEAR_CHAN_MAP::MAX_CHAN)
    {
      error_count++;

      if (special_event_num(error_count))
        {
          // ouput using fprintf to better work with log processing of online monitoring

          stringstream serr;

          serr << "FvtxDCMChannelMap::getFvtxStrip - Error #"
              << error_count << " - invalid DAQ address " << linear_chan
              << " (packet_id=" << packet_id << ", " << "fem_id=" << fem_id
              << ", " << "chip=" << chip_id << ", " << "channel=" << dcm_channel
              << ")"<<endl ;

          printf("%s", serr.str().c_str());

        }

      return NULL;
    }

  FvtxStrip* out = _map[linear_chan];

  if (!out && _verbosity>1)
    {
      std::cout
          << "FvtxDCMChannelMap::getFvtxStrip - can not find this trip in mapping "
          << linear_chan << " (packet_id=" << packet_id << ", " << "fem_id="
          << fem_id << ", " << "chip=" << chip_id << ", " << "channel=" << dcm_channel
          << ")" << endl;
      return NULL;
    }

  if (out && _verbosity >= FVTXGEOM::SOME)
    out->print();

  return out;

}

//_______________________________________________________________________ 
void
FvtxDCMChannelMap::MapStrips()
{

//  FvtxCage *cage = pArm->get_cage( cage_id );
//  FvtxStation *station = 0;
//  FvtxSector *sector = 0;
//  FvtxColumn *column = 0;
//  FvtxStrip *strip = 0;
//  unsigned long int channel = 0;
//  map<unsigned long int, FvtxStrip *> channelMap;
//
//  for (int station_id = 0; station_id < FVTXGEOM::NumberOfStations; station_id++) {
//    station = cage->get_station(station_id);
//    if ( !station ) cerr << "FvtxDCMChannelMap::MapStrips station " << station_id << " not found " << endl;
//    for (int sector_id = 0; sector_id < FVTXGEOM::NumberOfSectors; sector_id++) {
//      sector = station->get_sector(sector_id);
//      if ( !sector ) cerr << "FvtxDCMChannelMap::MapStrips sector " << sector_id << " not found " << endl;
//      for (int column_id = 0; column_id < FVTXGEOM::NumberOfColumns; column_id++) {
//        column = sector->get_column(column_id);
//        if ( !column ) cerr << "FvtxDCMChannelMap::MapStrips column " << column_id << " not found " << endl;
//        for (unsigned int strip_id = 0; strip_id < column->get_n_strips(); strip_id++) {
//          strip = column->get_strip(strip_id);
//
//          if(strip)
//          {
//              channel = strip->get_packet_id()*1000000 + strip->get_fem_id()*100000 + strip->get_chip_id()*1000 + strip->get_dcm_channel();
//              channelMap.insert(pair<unsigned long int, FvtxStrip *>(channel, strip));
//          }
//        }
//      }
//    }
//  }
//
//  if( cage_id == 0 ) Cage0Map = channelMap;
//  else if( cage_id == 1 ) Cage1Map = channelMap;

  MapStrips_Ideal();

}

//_______________________________________________________________________

// ideal mapping
// author: Cesar Luiz da Silva <slash@rcf.rhic.bnl.gov>
class FvtxDCMMap
{
public:

  static const int
  get_first_packet(int arm)
  {
    if (arm == 0)
      return FIRSTPACKET_S;
    if (arm == 1)
      return FIRSTPACKET_N;
    std::cout << "No arm number " << arm << std::endl;
    return 0;
  }

  static const int
  get_arm(int packet_id)
  {
    return (packet_id < get_first_packet(1)) ? 0 : 1;
  }

  static const int
  get_cage(int packet_id)
  {
    int arm = get_arm(packet_id);
    int relative_pkt = packet_id - get_first_packet(arm);
    return relative_pkt / NROCS_CAGE / 2;
  }

  static const int
  get_ROC(int packet_id)
  {
    int arm = get_arm(packet_id);
    return (packet_id - get_first_packet(arm)) / 2;
  }

  static const int
  get_ROC_FEM(int packet_id)
  {
    int arm = get_arm(packet_id);
    return (packet_id - get_first_packet(arm)) % 2;
  }

  static const int
  get_station(int fem_id, int chip_id)
  {
    if (fem_id == 0 || fem_id == 3)
      return ((chip_id <= 26) ? 0 : 1);
    if (fem_id == 1 || fem_id == 2)
      return ((chip_id <= 26) ? 2 : 3);
    std::cout << "FvtxDCMMap: no station for fem_id=" << fem_id
        << " and chip_id=" << chip_id << std::endl;
    return 100;
  }

  static const int
  get_FEM_column(int fem_id)
  {
    if (fem_id == 0 || fem_id == 1)
      return 0;
    if (fem_id == 2 || fem_id == 3)
      return 1;
    std::cout << "no column for fem_id=" << fem_id << std::endl;
    return 100;
  }

  static const int
  get_ROC_column(int packet_id, int fem_id)
  {
    return get_ROC_FEM(packet_id) * 2 + get_FEM_column(fem_id);
  }

  static const int
  get_station_wedge(int packet_id, int fem_id)
  {
    return get_cage(packet_id) * NROCS_CAGE * NCOLUMNS_ROC
        + get_ROC(packet_id) * NCOLUMNS_ROC + get_ROC_column(packet_id, fem_id);
  }

  static const int
  get_sector(int packet_id, int fem_id)
  {
    const int roc_col = get_ROC_column(packet_id, fem_id);
    if (roc_col >= 4)
      return 100;
    const int col2sec[] =
      { 1, 0, 3, 2 };

    return (get_ROC(packet_id) % NROCS_CAGE) * NCOLUMNS_ROC + col2sec[roc_col];
  }

  static const int
  get_wedge_chip(int packet_id, int fem_id, int chip_id)
  {
    int station = get_station(fem_id, chip_id);
    if (station == 0 || station == 2)
      return chip_id;
    return chip_id - 26;
  }

  enum
  {
    FIRSTPACKET_S = FVTXOO_FEM::SOUTH_PACKET_ID_BASE
  };
  enum
  {
    FIRSTPACKET_N = FVTXOO_FEM::NORTH_PACKET_ID_BASE
  };
  enum
  {
    NPACKETS_ARM = FVTXOO_FEM::NPACKET_MAX_PER_ARM
  };
  enum
  {
    NROCS_CAGE = FVTXOO_FEM::NUMROC_PER_CAGE
  };
  enum
  {
    NCOLUMNS_ROC = FVTXOO_FEM::NUMCOL_PER_ROC
  };
};

//_________________________________________________________________
// hardcoded ideal mapping
void
FvtxDCMChannelMap::MapStrips_Ideal()
{
  cout
      << "FvtxDCMChannelMap::MapStrips_Ideal() - Initiate the mapping table with ideal maping"
      << endl;

  for (int packet_i = 0; packet_i < FVTXOO_FEM::NPACKET_MAX; packet_i++)
    {
      const int packet_id = FVTXOO_FEM::GET_FVTX_PACKET_ID(packet_i);

      const int arm = FvtxDCMMap::get_arm(packet_id);
      const int cage = FvtxDCMMap::get_cage(packet_id);

      if (arm >= 2)
        {
          cout << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing arm " << arm
              << endl;
          continue;
        }
      FvtxArm* pArm = FvtxGeom::get_arm(arm);

      if (cage >= 2)
        {
          cout << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing cage " << cage
              << " @ packet = " << packet_id << endl;
          continue;
        }
      FvtxCage *pCage = pArm->get_cage(cage);

      for (int fem_id = 0; fem_id < (int) FVTXOO_FEM::NUMCABLE; fem_id++)
        {
          const int sector = FvtxDCMMap::get_sector(packet_id, fem_id);

          for (int chip = 1; chip <= (int) FVTXOO_FEM::CHIP_FEM_SIZE;
              chip++)
            {

              const int station = FvtxDCMMap::get_station(fem_id, chip);

              const int chip_per_side = (station == 0) ? 5 : 13;
              const int column = ((chip - 1) % 26) / chip_per_side;
              const int chip_in_column = ((chip - 1) % 26) % chip_per_side;
              if (column > 1)
                continue; // there is no chip 11-26 in station 0

              FvtxStation * pSstation = pCage->get_station(station);
              if (!pSstation)
                {
                  cout
                      << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing Station "
                      << station << endl;
                  continue;
                }

              FvtxSector * pSector = pSstation->get_sector(sector);
              if (!pSector)
                {
                  cout
                      << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing Sector "
                      << sector << endl;
                  continue;
                }

              FvtxColumn * pColumn = pSector->get_column(column);
              if (!pColumn)
                {
                  cout
                      << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing Column (AKA Side) "
                      << column << endl;
                  continue;
                }

              for (int channel = 0;
                  channel < (int) FVTXOO_FEM::CHANNEL_CHIP_SIZE; channel++)
                {
                  //int strip = chip_in_column * 128 + channel;
                  int strip;
                  if (column == 0){
                    //strip = (chip_per_side-1-chip_in_column) * 128 + (127-channel);
                    strip = (chip_per_side-1-chip_in_column) * 128 + channel;
                  }
                  else{
                    //strip = (chip_per_side-1-chip_in_column) * 128 + channel;
                    strip = (chip_per_side-1-chip_in_column) * 128 + (127-channel);
                  }

                  FvtxStrip *pStrip = pColumn->get_strip(strip);
                  if (!pStrip)
                    {
                      cout
                          << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing Strip"
                          << strip << endl;
                      continue;
                    }

                  pStrip->set_packet_id(packet_id);
                  pStrip->set_fem_id(fem_id);
                  pStrip->set_chip_id(chip);
                  pStrip->set_dcm_channel(channel);

                  int linear_chan = FVTXOO_FEM::LINEAR_CHAN_MAP::GET_CHAN(
                  /*const int*/packet_id,
                  /*const int*/fem_id,
                  /*const int*/chip,
                  /*const int*/channel);
                  if (linear_chan < 0
                      || linear_chan > FVTXOO_FEM::LINEAR_CHAN_MAP::MAX_CHAN)
                    {
                      cout
                          << "FvtxDCMChannelMap::MapStrips_Ideal() - Missing LINEAR_CHAN_MAP "
                          << linear_chan << " (packet_id=" << packet_id << ", "
                          << "fem_id=" << fem_id << ", " << "chip=" << chip
                          << ", " << "channel=" << channel << ")" << endl;
                      continue;
                    }

                  _map[linear_chan] = pStrip;

                } //for (int channel

            } // for (int chip

        } //for(int fem_id

    } // for (int packet_i
}

//_________________________________________________________________
FvtxDCMChannelMap::ChannelId::ChannelId(void) :
    arm(0), cage(0), station(0), sector(0), column(0), strip(0), packet(0), fem(
        0), chip(0), channel(0)
{
}

//_________________________________________________________________
FvtxDCMChannelMap::ChannelId::ChannelId(const FvtxStrip& strip_ref) :
    arm(strip_ref.index().arm()), cage(strip_ref.index().cage()), station(
        strip_ref.index().station()), sector(strip_ref.index().sector()), column(
        strip_ref.index().column()), strip(strip_ref.get_strip_index()), packet(
        strip_ref.get_packet_id()), fem(strip_ref.get_fem_id()), chip(
        strip_ref.get_chip_id()), channel(strip_ref.get_dcm_channel())
{
}

//_________________________________________________________________
ostream&
operator <<(ostream& out, const FvtxDCMChannelMap::ChannelId& id)
{
  out << id.packet << " " << id.fem << id.chip << id.channel << " " << id.arm
      << " " << id.cage << " " << id.station << " " << id.sector << " "
      << id.column << " " << id.strip;
  return out;
}

//_________________________________________________________________
istream&
operator >>(istream& in, FvtxDCMChannelMap::ChannelId& id)
{
  in >> id.packet >> id.fem >> id.chip >> id.channel >> id.arm >> id.cage
      >> id.station >> id.sector >> id.column >> id.strip;
  return in;
}

//_______________________________________________________
//! return true if the event number is special, i.e. 1,2...5,10,20...50,100,200,...
bool
FvtxDCMChannelMap::special_event_num(const int event_num)
{
  const double significand = event_num/pow(10,(int)(log10(event_num)));

  if (fmod (significand,1) == 0 && significand<=5) return true;
  else return false;
}

