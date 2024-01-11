#ifndef __MRPCHODOUNPACK_HH__
#define __MRPCHODOUNPACK_HH__

//#include <boost/array.hpp>
#include <TRpcHodoHitMap.h>
#include <PHTimeServer.h>
#include <memory>

class Event;
class Packet;
class PHCompositeNode;

/*! \ingroup modules */
//! Unpacks raw rpc data, populates TRpcHodoHitMap IOC with TRpcHodoHit objects

class mRpcHodoUnpack
{
 public:

  mRpcHodoUnpack();
  virtual ~mRpcHodoUnpack(){}
  virtual PHBoolean event(PHCompositeNode*);
  void setverbosity(Int_t verb) { fVerbosity = verb; }
  
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
          _packet_id             <<
          "     channel_number " <<
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

  //! fill TMutHitMap with unpacked channels
  void fill_hit_map( Int_t fThisPacket );

  //! Function to convert the Global Channel Number to RPC strip number
  Int_t getRPCCh( Int_t fPacket, Int_t fGlobalChannel, Int_t &fStrip);//Run 10 (full North RPC3)

  //! parameter table
  //  const mMutUnpackPar* _mod_par;

  //! pointer to hit map
  TRpcHodoHitMap* _hit_map;

  //! PRDF decoded event
  Event* _event;

  //! module Timer
  PHTimeServer::timer _timer;

  Packet* _packet;

  Int_t fVerbosity;

};

#endif /* __MRPCHODOUNPACK_HH__ */
