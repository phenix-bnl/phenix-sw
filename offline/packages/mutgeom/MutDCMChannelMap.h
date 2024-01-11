#ifndef __MUTDCMCHANNELMAP_HH__
#define __MUTDCMCHANNELMAP_HH__

// $Id: MutDCMChannelMap.h,v 1.10 2009/05/26 16:37:11 hpereira Exp $ 
/*!
  \file MutDCMChannelMap.h
  \brief mapping between DCM channel and strips
  \author Nicki Bruner 
  \version $Revision: 1.10 $
  \date $Date: 2009/05/26 16:37:11 $
*/

#include <map>
#include "MUTGEOM.h"

class MutArm;
class MutStrip;

//! DCM channel map
class MutDCMChannelMap
{
  public:
  
  //! used to easily read/write channels to file
  class ChannelId
  {
    public:
    
    //! empty constructor
    ChannelId( void );
    
    //! construct from strip
    ChannelId( const MutStrip& strip );
    
    int arm;
    
    int station;
    
    int octant;
    
    int half_octant;
    
    int gap;
    
    int plane;
    
    int strip;
    
    int packet;
    
    int channel;
    
    //! streamer
    friend std::ostream& operator << (std::ostream&, const ChannelId& );
    
    //! streamer
    friend std::istream& operator >> (std::istream&, ChannelId& );
    
  };
  
  // utility to get DCM map bank name for a given arm/station
  static std::string bankName( int arm, int station, std::string tag="" );
  
  //! constructor
  MutDCMChannelMap()
  {}
  
  //! constructor
  MutDCMChannelMap(const MutArm *arm);
  
  //! destructor
  ~MutDCMChannelMap()
  {}
  
  //! get strip from channel ID
  MutStrip* getMutStrip(int packet_id, int dcm_channel);
    
  //! verbosity
  void setVerbosity( MUTGEOM::Verbosity verbosity )
  { _verbosity = verbosity; }
  
  private:

  //! create DCM to stip mapping for a given station
  void MapStrips(int stationNum);
  
  //! pointer to parent arm
  const MutArm* pArm;

  //! strip map
  typedef std::map<int, MutStrip* > StripMap;
  
  //! station 1 map
  StripMap Station1Map;

  //! station 2 map
  StripMap Station2Map;
  
  //! station 3 map
  StripMap Station3Map;
  
  //! verbosity
  MUTGEOM::Verbosity _verbosity;
  
};

#endif   /* __MUTDCMCHANNELMAP_HH__ */
