// $Id: mMutUnpack.cxx,v 1.36 2011/07/14 04:26:09 richi Exp $
/*
  \file mMutUnpack.cxx
  \brief Unpacks MUTR raw data
  \author  S.Kelly, H.Pereira
  \version $Revision: 1.36 $
  \date $Date: 2011/07/14 04:26:09 $
*/

// MUTOO headers
#include <MUTOO_FEM.h>
#include <MutStrip.h>
#include <mMutUnpack.h>
#include <mMutUnpackPar.h>
#include <TMutNode.h>
#include <TMutHitMap.h>
#include <PHException.h>
#include <MUTOO_FEM.h>
#include <MUTOO.h>
#include <PHTimer.h>
#include <TMutActivePacketSet.h>

// PHENIX headers
#include <MutGeom.h>
#include <Event.h>

/*! \ingroup modules */

// STL/BOOST
//
#include <iostream>
#include <string>

using namespace std;

//_______________________________________________________
mMutUnpack::mMutUnpack() :
  _timer(PHTimeServer::get()->insert_new("mMutUnpack"))
{
  MUTOO::TRACE("initializing module mMutUnpack",MUTOO::ALOT);
}

//_______________________________________________________
// Event method.
PHBoolean mMutUnpack::event(PHCompositeNode* top_node)
{

  /*
  Reset interface pointers
  Loop over packets [
  Set _packet and _chan_map data members
  Unpack data into data member buffers
  Fill TMutHit Map from data member buffers
  ]
  */
  
  // Timer
  _timer.get()->restart();

  try {
    // Reset IOC pointers
    set_interface_ptrs(top_node);
		
    // clear maps
    _hit_map->clear();
		
    // loop over packets found in events
    packet_loop();

  } catch (std::exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the hit map
  _timer.get()->stop();
  if (_mod_par->get_verbosity() >= MUTOO::ALOT) _hit_map->print();
  if (_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();

  return True;
}


//_______________________________________________________
void mMutUnpack::print_summary( ostream& out ) const
{
  if( _bad_packets_detid.empty() && _bad_packets_userword.empty() ) return;
  
  MUTOO::PRINT( out, "mMutUnpack::summary" );
  
  if( !_bad_packets_detid.empty() )
  {
    out << "Following packets had bad detector id: " << endl;
    for( set<int>::const_iterator iter = _bad_packets_detid.begin(); iter != _bad_packets_detid.end(); iter++ )
    { out << "  " << *iter << endl; }
  }
  
  if( !_bad_packets_userword.empty() )
  {
    out << "Following packets had bad detector id: " << endl;
    for( set<int>::const_iterator iter = _bad_packets_userword.begin(); iter != _bad_packets_userword.end(); iter++ )
    { out << "  " << *iter << endl; }
  }
  
  MUTOO::PRINT( out, "**" );

}

//_______________________________________________________
void mMutUnpack::packet_loop( void )
{

  // Loop over packets and set _packet and _chan_map
  for (_packet_id = PACKET_BASE; _packet_id < PACKET_BASE + NPACKET_MAX; ++_packet_id) {

    // Set packet id or try the next one
    if ((_packet = _event->getPacket(_packet_id)) == 0)	continue;
    if( _mod_par->get_verbosity() >= MUTOO::ALOT ) 
    {
      std::cout << "mMutUnpack::packet_loop - packet: " << _packet_id
        << " detID: 0x" << hex << _packet->iValue(0, "ID")
        << " user_word: 0x" << hex << _packet->iValue(7, "USERWORD")
        << dec << endl;
    }
		
    // check detector ID if requested
    if ( _mod_par->get_check_detector_id() && _packet->iValue(0, "ID") != MUTOO_FEM::CATHDETID ) {
      
      if( _mod_par->get_verbosity() >= MUTOO::SOME ) 
      {
        std::cout
          << "mMutUnpack::packet_loop -"
          << " packet: " << _packet_id
          << " bad detector id: " << hex << _packet->iValue(0, "ID") << dec << std::endl;
      }
      _bad_packets_detid.insert( _packet_id );
      delete _packet;
      continue;
    }
    
    // check last user word if requested
    int userword( _packet->iValue(7, "USERWORD") & 0xfffff );
    if ( _mod_par->get_check_user_word() && (    
      userword != MUTOO_FEM::FPGAVERSIONRUN4 &&		
      userword != MUTOO_FEM::FPGAVERSIONRUN5 &&  
      userword != MUTOO_FEM::FPGAVERSIONRUN7
      ))
    {

      if( _mod_par->get_verbosity() >= MUTOO::SOME ) 
      {
        std::cout
          << "mMutUnpack::packet_loop -"
          << " packet: " << _packet_id
          << " bad User Word: " << hex << _packet->iValue(7, "USERWORD") << dec << std::endl;
      }
      
      _bad_packets_userword.insert( _packet_id );
      delete _packet;
      continue;
      
    }

    // Found this packet so we add to the active packet set
    TMutActivePacketSet::add_packet_id(_packet_id);
    
    // Set channel map pointer
    _chan_map = (_packet_id <= SOUTH_PACKET_MAX) ? SouthArmChannelMap() : NorthArmChannelMap();

    if (!_chan_map) {
      delete _packet;
      continue;
    }

    // Unpack data into _data, _chn, and _wrdcnt buffers
    unpack_channel_data();
    
    // Fill TMutHit map with this packets data
    fill_hit_map();

    // Ownership of packet was transfered to us, so we are
    // responsible for deleting it
    delete _packet;
  }

  return;
}

//_______________________________________________________
/*! Reset IOC and external interface pointers */
void mMutUnpack::set_interface_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mMutUnpackPar>::find_node(top_node, "mMutUnpackPar");

  // hit map pointer
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node, "TMutHitMap");

  // event pointer
  _event = TMutNode<Event>::find_node(top_node, "PRDF");
    
}

//_______________________________________________________
void mMutUnpack::unpack_channel_data()
{

  //	 1) Clear and initialize buffers
  //	 2) Fill Event lib buffers (_data,_chn,_wrdcnt)

  int status = 0;

  _data.assign(0);
  _chn.assign(0);
  _wrdcnt.assign(0);
  
  // Fill _data array (actual adc vals stored here)
  // MGW 2/12/2007 - this is wrong!  fillIntArray wants to know how
  //                 long our array is, NOT how long the data to fill is.  
  //                 It is simply doing a bounds check.
  // const int length = _packet->getDataLength();

  int data_words = 0;
  status = _packet->fillIntArray(&(_data[0]), _data.size(), &data_words, "SPARSE");
  if (status != 0) {
    delete _packet;
    throw std::runtime_error(DESCRIPTION("fillIntArray -- status: " + status));
  }

  // calculate the number of active channels, set data member
  _n_channel = data_words / NSAMPLES;
  if ( _mod_par->get_verbosity() >= MUTOO::MAX ) 
  {
    cout << "mMutUnpack::unpack_channel_data - got " << _n_channel
      << " channel(s) for packet " << _packet_id << endl;
  }
  
  // Fill _chn array (channel number stored here)
  int chn_words = 0;
  status = _packet->fillIntArray(&(_chn[0]), _chn.size(), &chn_words, "CHN");
  if (status != 0) 
  {
    delete _packet;
    throw std::runtime_error(DESCRIPTION("fillIntArray -- status: " + status));
  }
  
  // Fill _wrdcnt array (sample number associated with adc vals stored here)
  int wrdcnt_words = 0;
  status = _packet->fillIntArray(&(_wrdcnt[0]), _wrdcnt.size(), &wrdcnt_words, "WRDCNT");
  if (status != 0) 
  {
    delete _packet;
    throw std::runtime_error(DESCRIPTION("fillIntArray -- status: " + status));
  }
  return;
}

//_______________________________________________________
void mMutUnpack::fill_hit_map()
{

  for (int ichan = 0; ichan < _n_channel; ++ichan) 
  {

    // Index locates begining of this channels data
    // in _data, _chn, and _wrdcnt buffers.
    
    int index = ichan * NSAMPLES;
    
    // Use channel number to lookup up strip information
    // in _chan_map
    //
    int channel_number = _chn.at(index);
    
    MutStrip* strip = 0;
    
    try {
      // Get the strip pointer for this channel
      strip = _chan_map->getMutStrip(_packet_id, channel_number);
      
      // Null strip returned by channel map -- throw
      if (!strip) continue;
      
    } catch (channel_map_error& e) {
      MUTOO::TRACE(e.what());
      continue;
    }

    // Create new entry in TMutHitMap
    int cathode = (strip->getPlane() == 0) ? 0 : 1;
    TMutHitMap::iterator hit_iter = _hit_map->insert_new(
      strip->getArm(),
      strip->getStation(),
      strip->getOctant(),
      strip->getHalfOctant(),
      strip->getGap(),
      cathode,
      strip->getStrip());
    
    // Set TMutHit data fields.
    for (int i = 0; i < NSAMPLES; ++i) 
    {
      hit_iter->get()->set_amu(i, _packet->iValue(i, "AMU"));
      
      // Samples are not order 0,1,2,3 in data so we have
      // to look up the sample number in _wrdcnt buffer
      int isample = _wrdcnt.at(index + i);
      
      // Look up data word in _data buffer
      int data_word = _data.at(index + i);
      
      // Fill adc data member
      hit_iter->get()->set_adc(isample, data_word);
      
      if ( _mod_par->get_verbosity() >= MUTOO::MAX ) 
      {
        cout << "mMutUnpack::fill_hit_map - got adc " << data_word
          << " for sample " << isample
          << ", packet " << _packet_id
          << endl;
      }
    }
    
    // check packet id and dump corresponding hit key
    // if( _packet_id == 11179 || _packet_id == 11180 )
    // { cout << "mMutUnpack::fill_hit_map - packet: " << _packet_id << " hit: " <<  hit_iter->get()->get_key().get_obj_key() << endl; }
          
  }
  return;
}
