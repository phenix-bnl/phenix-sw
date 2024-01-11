// $Id: MutDCMChannelMap.C,v 1.14 2009/05/26 16:37:11 hpereira Exp $ 
/*!
  \file MutDCMChannelMap.C
  \brief mapping between DCM channel and strips
  \author Nicki Bruner 
  \version $Revision: 1.14 $
  \date $Date: 2009/05/26 16:37:11 $
*/

#include <map>
#include <sstream>

#include "MutArm.h"
#include "MutDCMChannelMap.h"
#include "MutStrip.h"

using namespace std;
using namespace MUTGEOM;

//_______________________________________________________________________
string MutDCMChannelMap::bankName( int arm, int station, string tag )
{

  // create filename if none is specified
  ostringstream what;
  what << "mut.";
  switch( station )
  {
    case MUTGEOM::Station1: 
    what << "St1ChannelMap";
    break;
    
    case MUTGEOM::Station2: 
    what << "St2ChannelMap";
    break;
    
    case MUTGEOM::Station3: 
    what << "St3ChannelMap";
    break;
    
    default:
    cout << "MutDCMChannelMap::bankName - unrecognized station id: " << station << endl;
    break;
  }
    
  what << "." << ( arm == MUTGEOM::South ? "South":"North" );
  
  if( !tag.empty() ) what << "." << tag;
  return what.str();
  
}

//_______________________________________________________________________
MutDCMChannelMap::MutDCMChannelMap( const MutArm* arm ):
  pArm( arm ),
  _verbosity( MUTGEOM::NONE )
{ for(int i=0; i<MUTGEOM::NumberOfStations; i++)  MapStrips(i); }

//_______________________________________________________________________
MutStrip* MutDCMChannelMap::getMutStrip(int packet_id, int dcm_channel)
{

  int channel = packet_id*1000 + dcm_channel;
  MutStrip* out = 0;
  if((packet_id >= 11001 && packet_id <= 11040) || (packet_id >= 11171 && packet_id <= 11210)) out = Station1Map[channel];
  else if((packet_id >= 11041 && packet_id <= 11104) || (packet_id >= 11211 && packet_id <= 11282)) out = Station2Map[channel];
  else if((packet_id >= 11105 && packet_id <= 11168) || (packet_id >= 11283 && packet_id <= 11362)) out = Station3Map[channel];

  if( out && _verbosity >= MUTGEOM::SOME ) out->printLocation();
  
  return out;
  
}

//_______________________________________________________________________ 
void MutDCMChannelMap::MapStrips(int stationNum)
{
  
  MutStation *pStation = pArm->f_pMutStations[stationNum];
  MutHalfOctant *pHalfOctant = 0;
  MutPlane *pPlane = 0;
  MutStrip *pStrip = 0;
  int Channel = 0;
  map<int, MutStrip *> channelMap;

  for(int i=0; i<NumberOfOctants; i++) 
  {
    for(int j=0; j<NumberOfHalfOctants; j++) 
    {
      pHalfOctant=pStation->f_pMutOctants[i]->f_pMutHalfOctants[j];
      int NumberOfGaps=pHalfOctant->getNumberOfGaps();
      for(int k=0; k<NumberOfGaps; k++) 
      {
        for(int l=0; l<NumberOfPlanes; l=l+2)
        {
          pPlane=pHalfOctant->f_pMutGaps[k]->f_pMutPlanes[l];
          for(int m=0; m<pPlane->getNumElements(); m++) 
          {
            pStrip = pPlane->f_pMutStrips[m];
            if(pStrip)
            {
              Channel = pStrip->getPacket_ID()*1000 + pStrip->getDCMChannel();
              channelMap.insert(pair<int, MutStrip *>(Channel, pStrip));
            }
          }
        }
      }
    }
  }
  
  if(stationNum==Station1) Station1Map=channelMap;
  else if(stationNum==Station2) Station2Map=channelMap;
  else if(stationNum==Station3) Station3Map=channelMap;

}

//_________________________________________________________________
MutDCMChannelMap::ChannelId::ChannelId( void ):
  arm(0),
  station(0),
  octant(0),
  half_octant(0),
  gap(0),
  plane(0),
  packet(0),
  channel(0)
{}

//_________________________________________________________________
MutDCMChannelMap::ChannelId::ChannelId( const MutStrip& strip_ref ):
  arm( strip_ref.getArm() ),
  station( strip_ref.getStation() ),
  octant( strip_ref.getOctant() ),
  half_octant( strip_ref.getHalfOctant() ),
  gap( strip_ref.getGap() ),
  plane( strip_ref.getPlane() ),
  strip( strip_ref.getStrip() ),
  packet( strip_ref.getPacket_ID() ),
  channel( strip_ref.getDCMChannel() )
{}

//_________________________________________________________________
ostream& operator << (ostream& out, const MutDCMChannelMap::ChannelId& id )
{
  out 
    << id.packet << " " << id.channel << " "
    << id.arm << " " << id.station << " " << id.octant << " " << id.half_octant << " "
    << id.gap << " " << id.plane << " " << id.strip;
  return out;
}


//_________________________________________________________________
istream& operator >> (istream& in, MutDCMChannelMap::ChannelId& id )
{
  in 
    >> id.packet >> id.channel 
    >> id.arm >> id.station >> id.octant >> id.half_octant 
    >> id.gap >> id.plane >> id.strip;
  return in;
}
