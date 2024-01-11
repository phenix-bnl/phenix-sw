// $Id: FvtxDCMChannelMap.h,v 1.4 2013/03/29 03:31:26 jinhuang Exp $

/*!
 * \file FvtxDCMChannelMap.h
 * \brief DAQ address -> FvtxStrip mapping
 * \version $Revision: 1.4 $
 * \date $Date: 2013/03/29 03:31:26 $
 */


#ifndef __FVTXDCMCHANNELMAP_HH__
#define __FVTXDCMCHANNELMAP_HH__



#ifndef __CINT__
#include<boost/array.hpp>
#endif
#include <map>
#include "FVTXGEOM.h"
#include "FVTXOO_FEM.h"

class FvtxArm;
class FvtxStrip;

//! DCM channel map
class FvtxDCMChannelMap
{
  public:
  
  //! used to easily read/write channels to file
  class ChannelId
  {
    public:
    
    //! empty constructor
    ChannelId( void );
    
    //! construct from strip
    ChannelId( const FvtxStrip& strip );
    
    int arm;

    int cage;
    
    int station;
    
    int sector;
    
    int column;
    
    int strip;
    
    int packet;

    int fem;

    int chip;
    
    int channel;
    
    //! streamer
    friend std::ostream& operator << (std::ostream&, const ChannelId& );
    
    //! streamer
    friend std::istream& operator >> (std::istream&, ChannelId& );
    
  };
  
  // utility to get DCM map bank name for a given arm/station
  static std::string bankName( int arm, int cage, std::string tag="" );
  
  //! constructor
  FvtxDCMChannelMap();
  
  //! constructor
//  FvtxDCMChannelMap();
  
  //! destructor
  ~FvtxDCMChannelMap()
  {}
  
  //! get strip from channel ID
  FvtxStrip* getFvtxStrip(int packet_id, int fem_id, int chip_id, int dcm_channel);
    
  //! verbosity
  void setVerbosity( FVTXGEOM::Verbosity verbosity )
  { _verbosity = verbosity; }
  
  private:

  //! create DCM to stip mapping for a given station
  void MapStrips();

  // Hardcoded mapping
  void MapStrips_Ideal();
  
//  //! pointer to parent arm
//  const FvtxArm* pArm;

#ifndef __CINT__
  //! strip map
  typedef boost::array<FvtxStrip*,
      FVTXOO_FEM::LINEAR_CHAN_MAP::MAX_CHAN > StripMap;
#else
  // CINT do not process boost::array?
  typedef FvtxStrip* StripMap[FVTXOO_FEM::LINEAR_CHAN_MAP::MAX_CHAN];
#endif

  StripMap _map;
  
  //! verbositymake install-data
  FVTXGEOM::Verbosity _verbosity;
  
  int error_count;

  //! return true if the event number is special, i.e. 1,2...5,10,20...50,100,200,...
  bool special_event_num(const int event_num);

};

#endif   /* __FVTXDCMCHANNELMAP_HH__ */
