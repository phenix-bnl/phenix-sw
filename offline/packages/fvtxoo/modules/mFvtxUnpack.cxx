// $Id: mFvtxUnpack.cxx,v 1.18 2016/04/20 13:56:18 pinkenbu Exp $
/*
 \file mFvtxUnpack.cxx
 \brief Unpacks FVTX raw data
 \author  Zhengyun You
 \version $Revision: 1.18 $
 \date $Date: 2016/04/20 13:56:18 $
 */

// FVTXOO headers
#include <FVTXOO_FEM.h>
#include <mFvtxUnpack.h>
#include <mFvtxUnpackPar.h>
#include <TMutNode.h>
#include <TFvtxHitMap.h>
#include <PHException.h>
#include <FVTXOO.h>
#include <PHTimer.h>
#include <TFvtxActivePacketSet.h>

// PHENIX headers
#include <RawDataCheck.h>
#include <FvtxGeom.h>
#include <Event.h>
#include <PHTFileServer.h>

/*! \ingroup modules */

// STL/BOOST
//
#include <iostream>
#include <string>
#include <cmath>
#include <cassert>

//ROOT
#include <TTree.h>
#include <TH1F.h>

using namespace std;

//_______________________________________________________
mFvtxUnpack::mFvtxUnpack() :
    _timer(PHTimeServer::get()->insert_new("mFvtxUnpack"))
{
  FVTXOO::TRACE("initializing module mFvtxUnpack");
  _hit_map = NULL;
  _mod_par = NULL;
  _event = NULL;
  _chan_map = NULL;
  _online_debug_tree = NULL;
  _event_num_odt = -1;
  _event_num_decoder_odt = -1;

  _save_online_info = false;
  _frame_check_mode = false;
  /*int * */_bco_odt = NULL;
  /*int * */_detID_odt = NULL;
  /*int * */_mod_odt = NULL;
  /*int * */_packet_odt = NULL;
  /*int * */_fem_id_odt = NULL;
  /*int * */_chip_odt = NULL;
  /*int * */_channel_odt = NULL;
  /*int * */_adc_value_odt = NULL;

  /*int * */_arm_odt = NULL;
  /*int * */_cage_odt = NULL;
  /*int * */_station_odt = NULL;
  /*int * */_sector_odt = NULL;
  /*int * */_column_odt = NULL;
  /*int * */_strip_odt = NULL;

  errcnt_paritycheck = 0;
  fem_errorcode = ErrCode_OK;
  errcnt_errorcode = 0;
  errcnt_errorcode_MissingFEMID = 0;
  errcnt_errorcode_MaxHitPerFEMID = 0;
  errcnt_errorcode_MaxHitPerPacket = 0;
  errcnt_errorcode_EvtNumErr = 0;
  cnt_event = 0;
  errcnt_avoid_fill_hit_map = 0;

  hEvtNoErr[0] = NULL;
  hEvtNoErr[1] = NULL;
  hBCOErr[0] = NULL;
  hBCOErr[1] = NULL;
}

//_______________________________________________________
mFvtxUnpack::~mFvtxUnpack()
{
  if (_bco_odt) delete[] _bco_odt; _bco_odt = NULL;
  if (_detID_odt) delete[] _detID_odt; _detID_odt = NULL;
  if (_mod_odt) delete[] _mod_odt; _mod_odt = NULL;
  if (_packet_odt) delete[] _packet_odt; _packet_odt = NULL;
  if (_fem_id_odt) delete[] _fem_id_odt; _fem_id_odt = NULL;
  if (_chip_odt) delete[] _chip_odt; _chip_odt = NULL;
  if (_channel_odt) delete[] _channel_odt; _channel_odt = NULL;
  if (_adc_value_odt) delete[] _adc_value_odt; _adc_value_odt = NULL;

  if (_arm_odt) delete[] _arm_odt; _arm_odt = NULL;
  if (_cage_odt) delete[] _cage_odt; _cage_odt = NULL;
  if (_station_odt) delete[] _station_odt; _station_odt = NULL;
  if (_sector_odt) delete[] _sector_odt; _sector_odt = NULL;
  if (_column_odt) delete[] _column_odt; _column_odt = NULL;
  if (_strip_odt) delete[] _strip_odt; _strip_odt = NULL;
}

// Process event
// input/out using direct pointers
//_______________________________________________________
PHBoolean
mFvtxUnpack::process_event(Event* e, TFvtxHitMap* hit_map)
{
  if (!e)
    cout << "mFvtxUnpack::process_event - Error cannot find input event data"
        << endl;
  if (!hit_map)
    cout << "mFvtxUnpack::process_event - Error cannot find input event data"
        << endl;


  _event = e;
  _hit_map = hit_map;

  try
    {
      packet_loop();

    }
  catch (std::exception& e)
    {
      FVTXOO::TRACE(e.what());
    }

  return True;
}

// Process event
// input/out using PHNodes
//_______________________________________________________
// Event method.
PHBoolean
mFvtxUnpack::process_event(PHCompositeNode* top_node)
{

  /*
   Reset interface pointers
   Loop over packets [
   Set _packet and _chan_map data members
   Unpack data into data member buffers
   Fill TFvtxHit Map from data member buffers
   ]
   */

  // Timer
  _timer.get()->restart();

  ++cnt_event;

  try
    {
      // Reset IOC pointers
      set_interface_ptrs(top_node);

      // loop over packets found in events
      packet_loop();

    }
  catch (std::exception& e)
    {
      FVTXOO::TRACE(e.what());
      return False;
    }

  // If verbose dump the contents of the hit map
  _timer.get()->stop();
  if (_mod_par->get_verbosity() >= FVTXOO::ALOT && _hit_map)
    _hit_map->print();
  if (_mod_par->get_verbosity() >= FVTXOO::SOME)
    _timer.get()->print();

  return True;
}

//_______________________________________________________
void
mFvtxUnpack::print_summary(ostream& out) const
{
  FVTXOO::PRINT(out, "mFvtxUnpack::summary");

  cout <<"Total event unpacked: \t"<<cnt_event<<endl;
  cout <<"Packet error count, all errors: \t"<<errcnt_errorcode<<endl;
  cout <<"Packet Error count, missing FEM ID: \t"<<errcnt_errorcode_MissingFEMID
      <<endl;
  cout <<"Packet Error count, Max hit per FEM ID: \t"<<errcnt_errorcode_MaxHitPerFEMID
      <<(errcnt_errorcode_MaxHitPerFEMID>0?" (Aborted reconstruction for events with these packets!!)":"")
      <<endl;
  cout <<"Packet Error count, Max hit per packet: \t"<<errcnt_errorcode_MaxHitPerPacket
      <<(errcnt_errorcode_MaxHitPerPacket>0?" (Aborted reconstruction for events with these packets!!)":"")
      <<endl;
  cout <<"Packet Error count, FEM event number error: \t"<<errcnt_errorcode_EvtNumErr
      <<endl;

  if (!_bad_packets_detid.empty())
    {
      out << "Following packets had bad detector id: " << endl;
      for (set<int>::const_iterator iter = _bad_packets_detid.begin();
          iter != _bad_packets_detid.end(); iter++)
        {
          out << "  " << *iter << endl;
        }
    }

  if (!_bad_packets_userword.empty())
    {
      out << "Following packets had bad detector id: " << endl;
      for (set<int>::const_iterator iter = _bad_packets_userword.begin();
          iter != _bad_packets_userword.end(); iter++)
        {
          out << "  " << *iter << endl;
        }
    }

  if (errcnt_paritycheck)
    out <<"Found "<<errcnt_paritycheck<<" Parity Errors"<<endl;
  if (errcnt_errorcode)
    out <<"Found "<<errcnt_errorcode<<" Decoder Errors"<<endl;


  FVTXOO::PRINT(out, "**");

}

//_______________________________________________________
void
mFvtxUnpack::packet_loop(void)
{

  if (!_event)
    cout << "mFvtxUnpack::packet_loop - Error cannot find input event data"
        << endl;
  if (!_mod_par)
    cout << "mFvtxUnpack::packet_loop - Error cannot find FvtxUnpack Parameter"
        << endl;
  if (_mod_par->get_verbosity() >= FVTXOO::SOME)
    cout << "mFvtxUnpack::packet_loop - Entry" << endl;

  // clear maps
  if (_hit_map)
    _hit_map->clear();
//  _strip_hits.clear();

  fem_errorcode = ErrCode_OK;

  _last_detID = -1;

  int first_bco[100] = {-1, -1, -1, -1}; // only the first four element is used, one for each cage

  //_online_debug_tree
  // TODO: this method seems to fold to 0 at 65536
//    _event_num_odt = _event -> getEvtSequence();
  // a quick and dirty fix:
  _event_num_odt++;
  _event_num_decoder_odt =  _event -> getEvtSequence();
  _nhits_odt = 0;

  // Loop over packets and set _packet and _chan_map
//  for (_packet_id = PACKET_BASE; _packet_id < PACKET_BASE + NPACKET_MAX; ++_packet_id) {
  //
  for (int packet_i = 0; packet_i < NPACKET_MAX; packet_i++)
    {

      // ---------------------------------------------------
      // Prepare for analysis
      // ---------------------------------------------------

      _packet_id = FVTXOO_FEM::GET_FVTX_PACKET_ID(packet_i);

      // re-initiate packet status arrays
      _pkt_paritycheck[packet_i] = 0;
      _pkt_nhits[packet_i] = 0;
      _pkt_data_length[packet_i] = 0;
      _pkt_errorcode[packet_i] = -1;
      _pkt_femerror[packet_i] = -1;
      _pkt_bco[packet_i] = -1;
      _pkt_event_num[packet_i] = -1;

      _avoid_fill_hit_map = false;

      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
        cout << "mFvtxUnpack::packet_loop - processing Packet #" << packet_i
            << " with _packet_id = " << _packet_id << endl;

      // Set packet id or try the next one
      if ((_packet = _event->getPacket(_packet_id)) == 0)
        continue;
      if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
        {
          std::cout << "mFvtxUnpack::packet_loop - packet: " << _packet_id
              << " length " << _packet->getLength() << " array length "
              << _packet->getArraylength() << " detID: 0x" << hex
              << _packet->iValue(0, "ID") << " user_word: 0x" << hex
              << _packet->iValue(7, "USERWORD") << dec << endl;
          _packet->dump(std::cout);
          //for (int iword = 0; iword < _packet->getLength()-6-5; iword++) cout << iword << "  " << _packet->iValue(iword) << endl;
        }

//    // check detector ID if requested
//    if ( _mod_par->get_check_detector_id() && _packet->iValue(0, "ID") != FVTXOO_FEM::FVTXDETID ) {
//
//      if( _mod_par->get_verbosity() >= FVTXOO::SOME )
//      {
//        std::cout
//          << "mFvtxUnpack::packet_loop -"
//          << " packet: " << _packet_id
//          << " bad detector id: " << hex << _packet->iValue(0, "ID") << dec << std::endl;
//      }
//      _bad_packets_detid.insert( _packet_id );
//      delete _packet;
//      continue;
//    }

//    // check last user word if requested
//    int userword( _packet->iValue(7, "USERWORD") & 0xfffff );
//    if ( _mod_par->get_check_user_word() && (
//      userword != FVTXOO_FEM::FPGAVERSIONRUN12
//      ))
//    {
//
//      if( _mod_par->get_verbosity() >= FVTXOO::SOME )
//      {
//        std::cout
//          << "mFvtxUnpack::packet_loop -"
//          << " packet: " << _packet_id
//          << " bad User Word: " << hex << _packet->iValue(7, "USERWORD") << dec << std::endl;
//      }
//
//      _bad_packets_userword.insert( _packet_id );
//      delete _packet;
//      continue;
//
//    }

//    // Found this packet so we add to the active packet set
//    TFvtxActivePacketSet::add_packet_id(_packet_id);

      // Set channel map pointer



      // ---------------------------------------------------
      // Unpack data into _data, _chn, and _wrdcnt buffers
      // ---------------------------------------------------
//    unpack_channel_data_old();

      if (!_frame_check_mode)
        unpack_channel_data_dcm0();

      // ---------------------------------------------------
      // Packet Status Checks
      // ---------------------------------------------------
      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
        {
          cout << "mFvtxUnpack::packet_loop - Checking decoder error for packet #"
          <<packet_i;
          _packet -> identify(cout);
          cout << endl;
        }

      // so decoder has been called and we archive error information
//      _pkt_nhits[packet_i]              = _packet->iValue(0, "HITS");
      _pkt_nhits[packet_i]              = _n_channel; // will be zero for _frame_check_mode = 1
      _pkt_data_length[packet_i]        = _packet -> getDataLength();
      _pkt_paritycheck[packet_i]        = _packet -> iValue(0,"CHECKPARITY");
      _pkt_bco[packet_i]                = _packet -> iValue(0,"BCLCK");
      _pkt_event_num[packet_i]          = _packet -> iValue(0,"EVTNR");

      // error check 1: decoder error
      _pkt_errorcode[packet_i] = _packet -> iValue(0,"ERRORCODE");
      _pkt_femerror[packet_i] = _packet -> iValue(0,"FEMERROR");

      // error check 2: unpacker error
      const int ev_diff = (_event_num_decoder_odt-_pkt_event_num[packet_i])&0xFFFF;
      if (
          ev_diff != 0 // for stand alone run
          && //
          ev_diff != 1 // for big partition run
      )
        { // event sync error

          _pkt_errorcode[packet_i] += ErrCode_EvtNumErr;

          _avoid_fill_hit_map = true;
          errcnt_avoid_fill_hit_map++;
          if (0 == errcnt_avoid_fill_hit_map%(int)(pow(10,floor(log(errcnt_avoid_fill_hit_map)/log(10)))))
            {
              cout << "mFvtxUnpack::packet_loop - Ignored "
                  <<errcnt_avoid_fill_hit_map
                  <<" packets for filling TFvtxHitMap. "
                  <<"Last error : packet event cnt "
                  <<_pkt_event_num[packet_i]<<" is inconsistent with decoder event # "
                  <<_event_num_decoder_odt << " on packet ";
              _packet -> identify(cout);
            }
        }

      // do some statistics on the error code
      switch (_pkt_errorcode[packet_i])
        {
      case ErrCode_MissingFEMID:
        errcnt_errorcode_MissingFEMID++;
//        fem_errorcode = ErrCode_MissingFEMID; // do not report this error to drop events
        break;
      case ErrCode_MaxHitPerFEMID:
        errcnt_errorcode_MaxHitPerFEMID++;
        fem_errorcode = ErrCode_MaxHitPerFEMID;
        break;
      case ErrCode_MaxHitPerPacket:
        errcnt_errorcode_MaxHitPerPacket++;
        fem_errorcode = ErrCode_MaxHitPerPacket;
        break;
      case ErrCode_EvtNumErr:
        errcnt_errorcode_EvtNumErr++;
//        fem_errorcode = errcnt_errorcode_EvtNumErr; // do not report this error to drop events
        break;
      default:
        break;
        }

      if (ev_diff && _event_num_decoder_odt >=0)
        { // event no error
          if (packet_i < NPACKET_MAX/2 && hEvtNoErr[0])
            {
              // north
              hEvtNoErr[0]->Fill(_packet_id-FVTXOO_FEM::SOUTH_PACKET_ID_BASE +1);
            }
          else if (hEvtNoErr[1])
            {
              hEvtNoErr[1]->Fill(_packet_id-FVTXOO_FEM::NORTH_PACKET_ID_BASE +1);
            }
        }


//      const int first_packet_cage = (int)(packet_i/FVTXOO_FEM::NPACKET_MAX_PER_CAGE) * FVTXOO_FEM::NPACKET_MAX_PER_CAGE;
      const int cage_idx = packet_i / FVTXOO_FEM::NPACKET_MAX_PER_CAGE;
      assert(cage_idx>=0 && cage_idx<4);
      if (first_bco[cage_idx]<0)  first_bco[cage_idx] = _pkt_bco[packet_i];

      if ( first_bco[cage_idx] != _pkt_bco[packet_i] && _event_num_decoder_odt >=0)
        { // GL1 sync error (bco inconsist)
          if (packet_i < NPACKET_MAX/2 && hBCOErr[0])
            {
              // north
              hBCOErr[0]->Fill(_packet_id-FVTXOO_FEM::SOUTH_PACKET_ID_BASE +1 );
            }
          else if (hBCOErr[1])
            {
              hBCOErr[1]->Fill(_packet_id-FVTXOO_FEM::NORTH_PACKET_ID_BASE +1 );
            }
        }

      // complain about parity mismatches
      if (_pkt_paritycheck[packet_i]<=0)
        {
          errcnt_paritycheck++;

          if (0 == errcnt_paritycheck%(int)(pow(10,floor(log(errcnt_paritycheck)/log(10)))))
            {
              cout << "mFvtxUnpack::packet_loop - Found "<<errcnt_paritycheck
                  <<" parity errors. Last error in ";
              _packet -> identify(cout);
            }
        } //  if (_pkt_paritycheck[packet_i]<=0)

      // complain about errors
      if (_pkt_errorcode[packet_i]>0 || _pkt_femerror[packet_i]>0)
        {
          errcnt_errorcode++;

          if (FVTXOO::special_event_num(errcnt_errorcode))
            {
              cout << "mFvtxUnpack::packet_loop - Decoder/Unpacker found "<<errcnt_errorcode
                  <<" errors. Last error : " //
                  <<"FEM Error Code = "<<_pkt_femerror[packet_i] //
                                                            <<", Decoder Error Code = "<<_pkt_errorcode[packet_i] //
                                                            <<" in ";
              _packet -> identify(cout);
            }
        } // if (_pkt_errorcode[packet_i]>0)

      // Fill hit maps for higher level reconstruction
      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
        {
          cout << "mFvtxUnpack::packet_loop - starting fill_hit_map() for packet #"
          <<packet_i;
          _packet -> identify(cout);
          cout << endl;
        }
      // ---------------------------------------------------
      // Fill TFvtxHit map with this packets data
      // ---------------------------------------------------
      if (!_frame_check_mode)
        fill_hit_map();

      if (_nhits_odt + _n_channel < HIT_SIZE_MAX)
        _nhits_odt += _n_channel;

      // ---------------------------------------------------
      // Ownership of packet was transfered to us, so we are
      // responsible for deleting it
      // ---------------------------------------------------
      delete _packet;
    } // for (int packet_i = 0;  packet_i< NPACKET_MAX; packet_i++)

  if (_online_debug_tree && _save_online_info)
    {
      if (_event_num_odt % 10000 == 0 || _mod_par->get_verbosity() >= FVTXOO::SOME)
        cout << "mFvtxUnpack::event - " << _event_num_odt
            << ", Event.getEvtSequence() = " << _event->getEvtSequence()
            << endl;
      _online_debug_tree -> Fill();
    }

//  // drop event for FVTX analysis if any packet failed FEM checks.
//  if (get_fem_errorcode() != ErrCode_OK)
//    if (_hit_map)
//      _hit_map->clear();

  return;
}

//_______________________________________________________
/*! Reset IOC and external interface pointers */
void
mFvtxUnpack::set_interface_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mFvtxUnpackPar>::find_node(top_node, "mFvtxUnpackPar");

  // hit map pointer
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node, "TFvtxHitMap");

  // event pointer
  _event = TMutNode<Event>::find_node(top_node, "PRDF");

}

//_______________________________________________________
void
mFvtxUnpack::unpack_channel_data_old()
{
  //     1) Clear and initialize buffers
  //     2) Fill Event lib buffers (_data,_chn,_wrdcnt)

  _fem_id.assign(0);
  _chip_id.assign(0);
  _channel_id.assign(0);
  _adc.assign(0);
  _wrdcnt.assign(0);

  /*
   // Fill _data array (actual adc vals stored here)
   // MGW 2/12/2007 - this is wrong!  fillIntArray wants to know how
   //                 long our array is, NOT how long the data to fill is.
   //                 It is simply doing a bounds check.
   // const int length = _packet->getDataLength();

   int data_words = 0;
   int status = 0;

   status = _packet->fillIntArray(&(_data[0]), _data.size(), &data_words, "SPARSE");
   if (status != 0) {
   delete _packet;
   throw std::runtime_error(DESCRIPTION("fillIntArray -- status: " + status));
   }

   // calculate the number of active channels, set data member
   _n_channel = data_words / NSAMPLES;
   if ( _mod_par->get_verbosity() >= FVTXOO::MAX )
   {
   cout << "mFvtxUnpack::unpack_channel_data - got " << _n_channel
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
   */

  // Use the packet_FVTX to extract packet when it is ready in online_distribution.
  // Now use the raw data format.
  int length = _packet->getLength();
  int nword_packet_header = 6;
  int nword_header = 5;
  int nword_trailer = 2;
  int nword_data = length - nword_packet_header - nword_header - nword_trailer;

  /*      _packet->fullIdentify(cout);
   cout << _packet->iValue(0, "EVTNR") << "  "
   << _packet->iValue(0, "DETID") << "  "
   << _packet->iValue(0, "MODADDR") << "  "
   << _packet->iValue(0, "FLAG") << "  "
   << endl;
   _packet->dump(cout);
   */
  if (_mod_par->get_verbosity() >= FVTXOO::MAX)
    for (int iword = 0; iword < _packet->getLength() - 6 - 5; iword++)
      cout << "mFvtxUnpack::unpack_channel_data_old - " << iword << "  "
          << _packet->iValue(iword) << endl;

  /*  int iarr[1024];
   int nlen = _packet->getLength();
   int nwout = 0;
   cout << "nlen " << nlen << endl;
   _packet->fillIntArray( &iarr[0], nlen, &nwout, "");
   for (int i = 0; i < nlen; i++) cout << iarr[i] << endl;
   */

  _n_channel = 0;
  int nword = 0;
  int fem_id = 0;
  int chip_id = 0;
  int channel_id = 0;
  int adc = 0;
  while (nword < nword_data + 1)
    {
      int data_word = _packet->iValue(nword);

      int is_trailer = data_word & 0xffff;
      if (!is_trailer)
        break; // this is the trailor word

      int is_fem = data_word & 0x0fff;
      if (!is_fem) // this is a FEM word
        {
          fem_id = (data_word & 0xf000) >> 12; // the high 4 bits of FEM_ID word (16 bit)
        }
      else // this is a DATA word
        {
          adc = data_word & 0x0007;
          chip_id = (data_word & 0x01f8) >> 3;
          channel_id = (data_word & 0xfe00) >> 9;

          _fem_id[_n_channel] = fem_id;
          _chip_id[_n_channel] = chip_id;
          _channel_id[_n_channel] = channel_id;
          _adc[_n_channel] = adc;
          _n_channel++;

          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
            cout << "mFvtxUnpack::unpack_channel_data_old - fem_id " << fem_id
                << " chip_id " << chip_id << " channel_id " << channel_id
                << " adc " << adc << endl;
        }
      nword++;
    }

  return;
}

//_______________________________________________________
void
mFvtxUnpack::unpack_channel_data_dcm0()
{
  //     1) Clear and initialize buffers
  //     2) Fill Event lib buffers (_data,_chn,_wrdcnt)

  _fem_id.assign(0);
  _chip_id.assign(0);
  _channel_id.assign(0);
  _adc.assign(0);
  _wrdcnt.assign(0);

  const int bco = _packet->iValue(0, "BCLCK");

  // Jin Huang <jhuang@bnl.gov>
  // Odd FEMs, which do not carry ROC SC fiber, do not have updated Mod Word
  if (_packet_id % 2 == 1)
    {
      _last_detID = _packet->iValue(0, "DETID");
    }
  const int detID = _last_detID;
  const int mod = _packet->iValue(0, "MODADDR");

  _n_channel = _packet->iValue(0, "HITS");
  if (_n_channel >= PACKET_SIZE_MAX)
    {
      cout
          << "mFvtxUnpack::unpack_channel_data_dcm0 - reached max hit number per event @ nhit = "
          << _n_channel << endl;
      _n_channel = PACKET_SIZE_MAX - 1;
    }

  for (int i = 0; i < _n_channel; i++)
    {
      _fem_id[i] = _packet->iValue(i, 0);
      _chip_id[i] = _packet->iValue(i, 1);
      _channel_id[i] = _packet->iValue(i, 2);
      _adc[i] = _packet->iValue(i, 3);

      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
        cout << "mFvtxUnpack::unpack_channel_data_dcm0 - fem_id " << _fem_id[i]
            << " chip_id " << _chip_id[i] << " channel_id " << _channel_id[i]
            << " adc " << _adc[i] << endl;

      if (_save_online_info)
        { // fill _online_debug_tree

          const int idx = i + _nhits_odt;
          if (idx < HIT_SIZE_MAX)
            {
              _bco_odt[idx] = bco;
              _detID_odt[idx] = detID;
              _mod_odt[idx] = mod;

              _packet_odt[idx] = _packet_id;
              _fem_id_odt[idx] = _fem_id[i];
              _chip_odt[idx] = _chip_id[i];
              _channel_odt[idx] = _channel_id[i];
              _adc_value_odt[idx] = _adc[i];
            }
        }

    }

  return;
}

//_______________________________________________________
void
mFvtxUnpack::fill_hit_map()
{

  for (int ichan = 0; ichan < _n_channel; ++ichan)
    {
      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
        cout << "mFvtxUnpack::fill_hit_map - trying to map hit #" << ichan
            << endl;

      if (_save_online_info)
        { // fill _online_debug_tree

          const int idx = ichan + _nhits_odt;
          if (idx < HIT_SIZE_MAX)
            {
              _arm_odt[idx] = -1;
              _cage_odt[idx] = -1;
              _station_odt[idx] = -1;
              _sector_odt[idx] = -1;
              _column_odt[idx] = -1;
              _strip_odt[idx] = -1;
            }
        }

      // Index locates begining of this channels data
      // in _data, _chn, and _wrdcnt buffers.
      // int index = ichan * NSAMPLES;

      // Use channel number to lookup up strip information
      // in _chan_map
      //
      int fem_id = _fem_id.at(ichan);
      int chip_id = _chip_id.at(ichan);
      int channel_id = _channel_id.at(ichan);
      int adc = _adc.at(ichan);

      float charge = adc < 7 ?
           (FVTXOO::FPHX_THRESH[adc] + FVTXOO::FPHX_THRESH[adc+1])/2.0 :
           (FVTXOO::FPHX_THRESH[adc] + FVTXOO::FPHX_MAXCHARGE)/2.0 ;

      FvtxStrip* strip = 0;

      try
        {
          // Get the strip pointer for this channel
          if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
            cout
                << "mFvtxUnpack::fill_hit_map - looking in FvtxDCMChannelMap for packet_id "
                << _packet_id << " fem_id " << fem_id << " chip_id " << chip_id
                << " channel_id " << channel_id << " adc " << adc << endl;

          strip = get_chan_map()->getFvtxStrip(_packet_id, fem_id, chip_id,
              channel_id);

          // Null strip returned by channel map -- throw
          if (!strip)
            continue;
          if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
            cout << "mFvtxUnpack::fill_hit_map - found strip index "
                << strip->index() << " strip " << strip->get_strip_index()
                << endl;

        }
      catch (channel_map_error& e)
        {
          FVTXOO::TRACE(e.what());
          continue;
        }

//      _strip_hits.push_back(*strip);

      // Create new entry in TFvtxHitMap
      if (_hit_map && !_avoid_fill_hit_map)
        {
          TFvtxHitMap::iterator hit_iter = _hit_map->insert_new(
              strip->index().arm(), strip->index().cage(),
              strip->index().station(), strip->index().sector(),
              strip->index().column(), strip->get_strip_index());
          // Fill adc data member
          hit_iter->get()->set_adc(adc);
          hit_iter->get()->set_q(charge);
        }

      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
        {
          cout << "mFvtxUnpack::fill_hit_map - got adc " << adc << endl;
        }

      // check packet id and dump corresponding hit key
      // if( _packet_id == 11179 || _packet_id == 11180 )
      // { cout << "mFvtxUnpack::fill_hit_map - packet: " << _packet_id << " hit: " <<  hit_iter->get()->get_key().get_obj_key() << endl; }

      if (_save_online_info)
        { // fill _online_debug_tree

          const int idx = ichan + _nhits_odt;
          if (idx < HIT_SIZE_MAX)
            {
              _arm_odt[idx] = strip->index().arm();
              _cage_odt[idx] = strip->index().cage();
              _station_odt[idx] = strip->index().station();
              _sector_odt[idx] = strip->index().sector();
              _column_odt[idx] = strip->index().column();
              _strip_odt[idx] = strip->get_strip_index();
            }
        }

    }
  return;
}

FvtxDCMChannelMap*
mFvtxUnpack::get_chan_map()
{
  if (_chan_map) return _chan_map;

  // Feb 2012, Jin Huang <jhuang@bnl.gov>
  // Redesign the mappings
  _chan_map = FvtxGeom::get_dcm_map();
//    _chan_map = (_packet_id <= SOUTH_PACKET_MAX) ? FvtxGeom::south_dcm_map() : FvtxGeom::north_dcm_map();

  if (!_chan_map)
    {
      cout <<"mFvtxUnpack::get_chan_map() - mFvtxUnpack::packet_loop - Missing Channel Map!!"<<endl;      // Need serious error

      throw std::runtime_error(DESCRIPTION("fail to initialize Channel Map"));
    }

  assert(_chan_map);

  _chan_map->setVerbosity(
      (FVTXGEOM::Verbosity) (_mod_par->get_verbosity()));

  return _chan_map;
}

bool
mFvtxUnpack::frame_check_mode(bool bframe_check)
{
  _frame_check_mode = bframe_check;

  if (_frame_check_mode)
    {
      cout
      <<"mFvtxUnpack::frame_check_mode - "
          <<"WARNING: Event frame check mode activated: "
          <<"Only packet header will be decoded. No hit information will be decoded or saved; nhits will be zero"
          <<endl;

    }

  return _frame_check_mode;
}

//_______________________________________________________
TTree *
mFvtxUnpack::save_online_debug_tree(string filename)
{
//  if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
//    {
//      cout << "mFvtxUnpack::save_online_debug_tree() - Entry"<<endl;
//    }

  if (_online_debug_tree)
    {
      cout
          << "mFvtxUnpack::save_online_debug_tree - The tree already exist, ignore new request for "
          << filename << endl;
      return _online_debug_tree;
    }

  if (filename.length() == 0)
    {
      cout << "mFvtxUnpack::save_online_debug_tree - invalid filename" << endl;
      return NULL;
    }

  PHTFileServer::get().open(filename.c_str(), "RECREATE");
  _root_file_name_odt = filename;

  _online_debug_tree = new TTree("online_debug_tree",
      "mFvtxUnpack::online_debug_tree Tree filled by event");

//  if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
//    {
//      cout << "mFvtxUnpack::save_online_debug_tree() - Initialize Tree"<<endl;
//    }

  if (!save_online_info())
  {
      throw std::runtime_error(
          DESCRIPTION("fail to initialize online information buffer"));
  }

  _online_debug_tree->Branch("event_num", &_event_num_odt, "event_num/I");
  _online_debug_tree->Branch("event_num_decoder", &_event_num_decoder_odt, "event_num_decoder/I");
  _online_debug_tree->Branch("nhits", &_nhits_odt, "nhits/I");

  n_packet = NPACKET_MAX;
  _online_debug_tree->Branch("n_packet", &n_packet, "n_packet/I");
  _online_debug_tree->Branch("nhits_packet", _pkt_nhits.data(), "nhits_packet[n_packet]/I");
  _online_debug_tree->Branch("data_length_packet", _pkt_data_length.data(), "data_length_packet[n_packet]/I");
  _online_debug_tree->Branch("paritycheck", _pkt_paritycheck.data(), "paritycheck[n_packet]/I");
  _online_debug_tree->Branch("errorcode", _pkt_errorcode.data(), "errorcode[n_packet]/I");
  _online_debug_tree->Branch("fem_error", _pkt_femerror.data(), "fem_error[n_packet]/I");
  _online_debug_tree->Branch("bco_packet", _pkt_bco.data(), "bco_packet[n_packet]/I");
  _online_debug_tree->Branch("event_num_packet", _pkt_event_num.data(), "event_num_packet[n_packet]/I");

  _online_debug_tree->Branch("bco", _bco_odt, "bco[nhits]/I");
  _online_debug_tree->Branch("detID", _detID_odt, "detID[nhits]/I");
  _online_debug_tree->Branch("mod", _mod_odt, "mod[nhits]/I");
  _online_debug_tree->Branch("packet", _packet_odt, "packet[nhits]/I");
  _online_debug_tree->Branch("fem_id", _fem_id_odt, "fem_id[nhits]/I");
  _online_debug_tree->Branch("chip", _chip_odt, "chip[nhits]/I");
  _online_debug_tree->Branch("channel", _channel_odt, "channel[nhits]/I");
  _online_debug_tree->Branch("adc_value", _adc_value_odt, "adc_value[nhits]/I");

  _online_debug_tree->Branch("arm", _arm_odt, "arm[nhits]/I");
  _online_debug_tree->Branch("cage", _cage_odt, "cage[nhits]/I");
  _online_debug_tree->Branch("station", _station_odt, "station[nhits]/I");
  _online_debug_tree->Branch("sector", _sector_odt, "sector[nhits]/I");
  _online_debug_tree->Branch("side", _column_odt, "side[nhits]/I");
  _online_debug_tree->Branch("strip", _strip_odt, "strip[nhits]/I");

  cout << "mFvtxUnpack::save_online_debug_tree - Save raw hits map to "
      << filename << endl;

//  if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
//    {
//      cout << "mFvtxUnpack::save_online_debug_tree() - Done"<<endl;
//    }



  hEvtNoErr[0] = new TH1F("hEvtNoErr_South","EvtNoErr: Event Num Check - South",
      24,.5,24.5);
  hEvtNoErr[1] = new TH1F("hEvtNoErr_North","EvtNoErr: Event Num Check - North",
      24,.5,24.5);

  hEvtNoErr[0] -> SetXTitle("packet - 25000");
  hEvtNoErr[0] -> SetYTitle("Error Ratio");
  hEvtNoErr[0] -> SetFillColor(kRed);

  hEvtNoErr[1] -> SetXTitle("packet - 25100");
  hEvtNoErr[1] -> SetYTitle("Error Ratio");
  hEvtNoErr[1] -> SetFillColor(kRed);

  hBCOErr[0] = new TH1F("hBCOErr_South","FemClkErr: BCO Check - South",
      FVTXOO_FEM::NPACKET_MAX_PER_ARM,.5,FVTXOO_FEM::NPACKET_MAX_PER_ARM+.5);
  hBCOErr[1] = new TH1F("hBCOErr_North","FemClkErr: BCO Check - North",
      FVTXOO_FEM::NPACKET_MAX_PER_ARM,.5,FVTXOO_FEM::NPACKET_MAX_PER_ARM+.5);

  hBCOErr[0] -> SetXTitle("packet - 25000");
  hBCOErr[0] -> SetYTitle("Error Ratio");
  hBCOErr[0] -> SetFillColor(kRed);

  hBCOErr[1] -> SetXTitle("packet - 25100");
  hBCOErr[1] -> SetYTitle("Error Ratio");
  hBCOErr[1] -> SetFillColor(kRed);

  return _online_debug_tree;
}

//_______________________________________________________
bool
mFvtxUnpack::save_online_info()
{

//  if (_mod_par->get_verbosity() >= FVTXOO::SOME)
//    {
//      cout << "mFvtxUnpack::save_online_info() - Entry with _save_online_info = "
//          <<_save_online_info<<endl;
//    }

  if ( _save_online_info ) return _save_online_info;

  /*int * */_bco_odt = new int[HIT_SIZE_MAX];
  if (!_bco_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_detID_odt = new int[HIT_SIZE_MAX];
  if (!_detID_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_mod_odt = new int[HIT_SIZE_MAX];
  if (!_mod_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_packet_odt = new int[HIT_SIZE_MAX];
  if (!_packet_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_fem_id_odt = new int[HIT_SIZE_MAX];
  if (!_fem_id_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_chip_odt = new int[HIT_SIZE_MAX];
  if (!_chip_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_channel_odt = new int[HIT_SIZE_MAX];
  if (!_channel_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_adc_value_odt = new int[HIT_SIZE_MAX];
  if (!_adc_value_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }

  /*int * */_arm_odt = new int[HIT_SIZE_MAX];
  if (!_arm_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_cage_odt = new int[HIT_SIZE_MAX];
  if (!_cage_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_station_odt = new int[HIT_SIZE_MAX];
  if (!_station_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_sector_odt = new int[HIT_SIZE_MAX];
  if (!_sector_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_column_odt = new int[HIT_SIZE_MAX];
  if (!_column_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }
  /*int * */_strip_odt = new int[HIT_SIZE_MAX];
  if (!_strip_odt) { throw std::runtime_error(DESCRIPTION("fail to initialize buffer")); return false; }

//  if (_mod_par->get_verbosity() >= FVTXOO::SOME)
//    {
//      cout << "mFvtxUnpack::save_online_info() - Successful"<<endl;
//    }

  return _save_online_info = true;
}

//_______________________________________________________
bool
mFvtxUnpack::write_online_debug_tree()
{
  if (_online_debug_tree)
    {
      if (hEvtNoErr[0])
        hEvtNoErr[0] -> Scale(1/(double)_event_num_odt);
      if (hEvtNoErr[1])
        hEvtNoErr[1] -> Scale(1/(double)_event_num_odt);

      if (hBCOErr[0])
        hBCOErr[0] -> Scale(1/(double)_event_num_odt);
      if (hBCOErr[1])
        hBCOErr[1] -> Scale(1/(double)_event_num_odt);

      cout << "mFvtxUnpack::write_online_debug_tree - save to "
          << _root_file_name_odt << endl;
      return PHTFileServer::get().write(_root_file_name_odt);
    }
  else
    return false;
}
