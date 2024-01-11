#ifndef __MMUTUNPACK_HH__
#define __MMUTUNPACK_HH__

#include <MUTOO_FEM.h>
#include <boost/array.hpp>
#include <TMutHitMap.h>
#include <PHTimeServer.h>
#include <memory>
#include <mMutUnpackPar.h>

class Event;
class MutDCMChannelMap;
class Packet;
class PHCompositeNode;

/*! \ingroup modules */
//! Unpacks raw data, populates TMutHitMap IOC with TMutHit objects.
/*! 
Unpacks raw MUTR hit information and populates TMutHitMap IOC.	The runtime
parameter table mMutUnpackPar specifies a zero suppression mode via the
enumeration mMutUnpackPar::ZeroSupMode. Currently available zero suppression
schemes are NONE, MONTE_CARLO and DCM.	Implementatation details are documented
in the file mMutUnpack.cxx.
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutUnpackPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutHitMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> Event* </td>
<td> External Interface </td>
<td> mutable </td>
</tr>
<tr>
<td> MutDCMChannelMap* </td>
<td> External Interface </td>
<td> mutable </td>
</tr>
</table>
*/
class mMutUnpack
{
 public: 

  mMutUnpack(); 
  virtual ~mMutUnpack(){} 
  virtual PHBoolean event(PHCompositeNode*);

  //! print summary of how-many hits have been removed
  void print_summary( std::ostream& out = std::cout ) const;
  
  class channel_map_error : public std::runtime_error {
    
    public:
    //! Construct with offending packet id and channel number
    channel_map_error(int packet_id, int channel_number) : 
      std::runtime_error("channel_map_error"),
      _packet_id(packet_id),
      _channel_number(channel_number)
    {;}

    //! Overrides std::runtime_error
    virtual const char * what () const throw ()
    {
      std::ostringstream what;
      what << "channel_map error: packet_id " << 
        _packet_id		 << 
        "	channel_number " <<
        _channel_number;
      return what.str().c_str();
    }		
    
    private:
    int _packet_id;
    int _channel_number;
  };
 
  private:	

  //! get local pointers to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! loop over all packets found in events; check; fill 
  void packet_loop( void );
  
  //! upack data for each channel found in event
  void unpack_channel_data( void );
  
  //! fill TMutHitMap with unpacked channels
  void fill_hit_map( void );

  //! parameter table
  const mMutUnpackPar* _mod_par;	
  
  //! pointer to hit map
  TMutHitMap* _hit_map;
  
  //! PRDF decoded event 
  Event* _event;						
  
  //! mutr channel map
  MutDCMChannelMap* _chan_map;
  
  //! module Timer
  PHTimeServer::timer _timer;
  
  // define some constants for readibility
  static const int NPACKET_MAX = MUTOO_FEM::NPACKET_MAX;
  static const int PACKET_BASE = MUTOO_FEM::PACKET_ID_BASE;
  static const int FEM_SIZE = MUTOO_FEM::CATH_FEM_SIZE;
  static const int PACKET_SIZE_MAX = MUTOO_FEM::PACKET_SIZE_MAX;
  static const int NSAMPLES = MUTOO_FEM::NSAMPLES;
  static const int SOUTH_PACKET_MAX = MUTOO_FEM::SOUTH_PACKET_MAX;

  // Event lib buffers
  boost::array<int,PACKET_SIZE_MAX> _data;
  boost::array<int,PACKET_SIZE_MAX> _chn;
  boost::array<int,PACKET_SIZE_MAX> _wrdcnt;
  
  // data members associated with this current packet
  int _n_channel;
  Packet* _packet;
  int _packet_id;
    
  //! list of packets with bad detector id
  std::set<int> _bad_packets_detid;
  
  //! list of packets with bad user word
  std::set<int> _bad_packets_userword;
  
};

#endif /* __MMUTUNPACK_HH__ */







