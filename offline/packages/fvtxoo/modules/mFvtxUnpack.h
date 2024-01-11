// $Id: mFvtxUnpack.h,v 1.11 2015/07/02 17:43:40 jinhuang Exp $

#ifndef __MFVTXUNPACK_HH__
#define __MFVTXUNPACK_HH__

#include <FVTXOO_FEM.h>
#include <boost/array.hpp>
#include <TFvtxHitMap.h>
#include <PHTimeServer.h>
#include <memory>
#include <string>
#include <mFvtxUnpackPar.h>
#include <FvtxStrip.h>
//#include <packet_A.h>

class Event;
class FvtxDCMChannelMap;
class Packet;
class PHCompositeNode;
class TTree;
class TH1;

/*! \ingroup modules */
//! Unpacks raw data, populates TFvtxHitMap IOC with TFvtxHit objects.
/*! 
 Unpacks raw MUTR hit information and populates TFvtxHitMap IOC.	The runtime
 parameter table mFvtxUnpackPar specifies a zero suppression mode via the
 enumeration mFvtxUnpackPar::ZeroSupMode. Currently available zero suppression
 schemes are NONE, MONTE_CARLO and DCM.	Implementatation details are documented
 in the file mFvtxUnpack.cxx.
 <br>
 <h2>Analysis Module Interface Specification</h2>
 <table>
 <tr>
 <td> Object </td>
 <td> Description </td>
 <td> Privilege </td>
 </tr>
 <tr>
 <td> const mFvtxUnpackPar*</td>
 <td> Parameter Table </td>
 <td> immutable </td>
 </tr>
 <tr>
 <td> TFvtxHitMap*</td>
 <td> IOC</td>
 <td> mutable </td>
 </tr>
 <tr>
 <td> Event* </td>
 <td> External Interface </td>
 <td> mutable </td>
 </tr>
 <tr>
 <td> FvtxDCMChannelMap* </td>
 <td> External Interface </td>
 <td> mutable </td>
 </tr>
 </table>
 */
class mFvtxUnpack
{
public:

  mFvtxUnpack();
  virtual
  ~mFvtxUnpack();

  virtual PHBoolean
  process_event(PHCompositeNode*);
  virtual PHBoolean
  process_event(Event* e, TFvtxHitMap* hit_map);

  //! print summary of how-many hits have been removed
  void
  print_summary(std::ostream& out = std::cout) const;

  TTree *
  save_online_debug_tree(std::string filename);
  bool
  write_online_debug_tree();
  bool frame_check_mode(bool bframe_check = true); // Enable Event Frame Check Mode

  class channel_map_error : public std::runtime_error
  {

  public:
    //! Construct with offending packet id and channel number
    channel_map_error(int packet_id, int channel_number) :
        std::runtime_error("channel_map_error"), _packet_id(packet_id), _channel_number(
            channel_number)
    {
      ;
    }

    //! Overrides std::runtime_error
    virtual const char *
    what() const throw ()
    {
      std::ostringstream whatres;
      whatres << "channel_map error: packet_id " << _packet_id
	      << "	channel_number " << _channel_number;
      return whatres.str().c_str();
    }

  private:
    int _packet_id;
    int _channel_number;
  };

  void
  set_unpacker_par(const mFvtxUnpackPar * mod_par)
  {
    _mod_par = mod_par;
  }

//  typedef std::vector<FvtxStrip> typ_strip_hits;
//  typ_strip_hits & get_strip_hits() {return _strip_hits;}

  enum {
    ErrCode_MissingPkt = -1, // missing packet
    ErrCode_OK = 0, //  no error
    ErrCode_MissingFEMID = 1, // Decoder Error: got data words without FEM ID
    ErrCode_MaxHitPerFEMID = 2, // Decoder Error: we aleady have reached the max hit count for FEMID
    ErrCode_MaxHitPerPacket = 3, // Decoder Error: we aleady have reached the max total hit count
    ErrCode_EvtNumErr = 10, // Unpack Error: event number misalignment
  };

  int get_fem_errorcode() const {return fem_errorcode;}


private:

  //! get local pointers to needed nodes
  void
  set_interface_ptrs(PHCompositeNode* top_node);

  //! loop over all packets found in events; check; fill
  void
  packet_loop(void);

  //! upack data for each channel found in event - old version
  void
  unpack_channel_data_old(void);

  // utilizing new fvtx decoder for 1008 data, IDFVTX_DCM0
  // Jan 2012, Jin Huang <jhuang@bnl.gov>
  void
  unpack_channel_data_dcm0(void);

  //! fill TFvtxHitMap with unpacked channels
  void
  fill_hit_map(void);

  //! parameter table
  const mFvtxUnpackPar* _mod_par;

  //! pointer to hit map
  TFvtxHitMap* _hit_map;

//  typ_strip_hits _strip_hits;

  //! PRDF decoded event 
  Event* _event;

  //! fvtx channel map
  FvtxDCMChannelMap* get_chan_map();
  FvtxDCMChannelMap* _chan_map;

  //! module Timer
  PHTimeServer::timer _timer;

  // define some constants for readability
  static const int NPACKET_MAX = FVTXOO_FEM::NPACKET_MAX;
//  static const int PACKET_BASE = FVTXOO_FEM::PACKET_ID_BASE;
  static const int SOUTH_PACKET_MAX = FVTXOO_FEM::SOUTH_PACKET_MAX;

  static const int PACKET_SIZE_MAX = FVTXOO_FEM::PACKET_SIZE_MAX;
  static const int FEM_SIZE = FVTXOO_FEM::CHIP_FEM_SIZE;
  static const int NSAMPLES = FVTXOO_FEM::NSAMPLES;

  // Event lib buffers
  boost::array<int, PACKET_SIZE_MAX> _fem_id;
  boost::array<int, PACKET_SIZE_MAX> _chip_id;
  boost::array<int, PACKET_SIZE_MAX> _channel_id;
  boost::array<int, PACKET_SIZE_MAX> _adc;
  boost::array<int, PACKET_SIZE_MAX> _wrdcnt;

  // Decoder Error Information
  boost::array<int, NPACKET_MAX> _pkt_paritycheck;
  int errcnt_paritycheck;

  boost::array<int, NPACKET_MAX> _pkt_errorcode; // decoding and unpacking error checks
  boost::array<int, NPACKET_MAX> _pkt_femerror; // decoding and unpacking error checks

  int fem_errorcode;

  int errcnt_errorcode;
  int errcnt_errorcode_MissingFEMID;
  int errcnt_errorcode_MaxHitPerFEMID;
  int errcnt_errorcode_MaxHitPerPacket;
  int errcnt_errorcode_EvtNumErr;
  int cnt_event;

  boost::array<int, NPACKET_MAX> _pkt_bco; // should be identical to BCO assigned to each hit
  boost::array<int, NPACKET_MAX> _pkt_event_num;
  boost::array<int, NPACKET_MAX> _pkt_nhits;
  boost::array<int, NPACKET_MAX> _pkt_data_length;
  int n_packet; // = NPACKET_MAX just to help filling online debug tree

  // data members associated with this current packet
  int _n_channel;
  Packet* _packet;
  int _packet_id;
  int _last_detID;
  bool _avoid_fill_hit_map;
  int errcnt_avoid_fill_hit_map;

  //! list of packets with bad detector id
  std::set<int> _bad_packets_detid;

  //! list of packets with bad user word
  std::set<int> _bad_packets_userword;

  // -----------------------------------------
  // export online debug information
  // for online monitoring and a TTree
  // -----------------------------------------
public:
  bool
  save_online_info();
  const int
  get_event_num() const
  {
    return _event_num_odt;
  }
  const int
  get_n_raw_hits() const
  {
    return _nhits_odt;
  }

  /*!  packet informations - parity check
  //
  // packet_i = 0-47 for packet 25001-25024, 25101-25124
  // Use FVTXOO_FEM::GET_FVTX_PACKET_ID(packet_i); to convert to packet ID
  // return: 1 = parity OK; -1 = parity inconsistency; 0 = wrong packet index input
   *
   */
  const int
  get_partiycheck(unsigned int packet_i)
  {
    if (packet_i < (unsigned int)NPACKET_MAX)
      {
        return _pkt_paritycheck[packet_i];
      }
    else
      return 0;
  }

  /*! packet informations - error code
  //
  // packet_i = 0-47 for packet 25001-25024, 25101-25124
  // Use FVTXOO_FEM::GET_FVTX_PACKET_ID(packet_i); to convert to packet ID
  // return:
  // 0 = No Error
  // 1 : got data words without FEM ID
  // 2 : we aleady have reached the max hit count for FEMID
  // 3 : we aleady have reached the max total hit count
   */
  const int
  get_errorcode(unsigned int packet_i)
  {
    if (packet_i < (unsigned int)NPACKET_MAX)
      {
        return _pkt_errorcode[packet_i];
      }
    else
      return ErrCode_MissingPkt;
  }

  /*! packet informations - FEM error code
  //
  // packet_i = 0-47 for packet 25001-25024, 25101-25124
  // Use FVTXOO_FEM::GET_FVTX_PACKET_ID(packet_i); to convert to packet ID
  // return:
  // 0 = No Error
  // -1= Missing packet
  // other number : Error observed on FEM, e.g. buffer full
  // Details see: http://www.phenix.bnl.gov/WWW/fvtx/DAQ/DataFormats/data_formats_all.xls / FEM Data / Control Word
   */
  const int
  get_femerror(unsigned int packet_i)
  {
    if (packet_i < (unsigned int)NPACKET_MAX)
      {
        return _pkt_femerror[packet_i];
      }
    else
      return -1;
  }

  const int
  get_bco(unsigned int raw_hit_index) const
  {
    return
        (_bco_odt && (int) raw_hit_index < _nhits_odt) ?
            _bco_odt[raw_hit_index] : -1;
  }
  const int
  get_detID(unsigned int raw_hit_index) const
  {
    return
        (_detID_odt && (int) raw_hit_index < _nhits_odt) ?
            _detID_odt[raw_hit_index] : -1;
  }
  const int
  get_mod(unsigned int raw_hit_index) const
  {
    return
        (_mod_odt && (int) raw_hit_index < _nhits_odt) ?
            _mod_odt[raw_hit_index] : -1;
  }
  const int
  get_packet(unsigned int raw_hit_index) const
  {
    return
        (_packet_odt && (int) raw_hit_index < _nhits_odt) ?
            _packet_odt[raw_hit_index] : -1;
  }
  const int
  get_fem_id(unsigned int raw_hit_index) const
  {
    return
        (_fem_id_odt && (int) raw_hit_index < _nhits_odt) ?
            _fem_id_odt[raw_hit_index] : -1;
  }
  const int
  get_chip(unsigned int raw_hit_index) const
  {
    return
        (_chip_odt && (int) raw_hit_index < _nhits_odt) ?
            _chip_odt[raw_hit_index] : -1;
  }
  const int
  get_channel(unsigned int raw_hit_index) const
  {
    return
        (_channel_odt && (int) raw_hit_index < _nhits_odt) ?
            _channel_odt[raw_hit_index] : -1;
  }
  const int
  get_adc_value(unsigned int raw_hit_index) const
  {
    return
        (_adc_value_odt && (int) raw_hit_index < _nhits_odt) ?
            _adc_value_odt[raw_hit_index] : -1;
  }

  const int
  get_arm(unsigned int raw_hit_index) const
  {
    return
        (_arm_odt && (int) raw_hit_index < _nhits_odt) ?
            _arm_odt[raw_hit_index] : -1;
  }
  const int
  get_cage(unsigned int raw_hit_index) const
  {
    return
        (_cage_odt && (int) raw_hit_index < _nhits_odt) ?
            _cage_odt[raw_hit_index] : -1;
  }
  const int
  get_station(unsigned int raw_hit_index) const
  {
    return
        (_station_odt && (int) raw_hit_index < _nhits_odt) ?
            _station_odt[raw_hit_index] : -1;
  }
  const int
  get_sector(unsigned int raw_hit_index) const
  {
    return
        (_sector_odt && (int) raw_hit_index < _nhits_odt) ?
            _sector_odt[raw_hit_index] : -1;
  }
  const int
  get_column(unsigned int raw_hit_index) const
  {
    return
        (_column_odt && (int) raw_hit_index < _nhits_odt) ?
            _column_odt[raw_hit_index] : -1;
  }
  const int
  get_strip(unsigned int raw_hit_index) const
  {
    return
        (_strip_odt && (int) raw_hit_index < _nhits_odt) ?
            _strip_odt[raw_hit_index] : -1;
  }

protected:
  bool _save_online_info;
  TTree * _online_debug_tree;
  std::string _root_file_name_odt;

  bool _frame_check_mode;
  TH1 * hEvtNoErr[2];
  TH1 * hBCOErr[2];

  static const int HIT_SIZE_MAX = PACKET_SIZE_MAX * NPACKET_MAX;
  int _event_num_odt;
  int _event_num_decoder_odt;
  int _nhits_odt;

  int * _bco_odt;
  int * _mod_odt;
  int * _detID_odt;
  int * _packet_odt;
  int * _fem_id_odt;
  int * _chip_odt;
  int * _channel_odt;
  int * _adc_value_odt;

  int * _arm_odt;
  int * _cage_odt;
  int * _station_odt;
  int * _sector_odt;
  int * _column_odt;
  int * _strip_odt;
};

#endif /* __MFVTXUNPACK_HH__ */

