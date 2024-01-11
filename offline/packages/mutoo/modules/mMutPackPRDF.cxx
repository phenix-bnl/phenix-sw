// $Id: mMutPackPRDF.cxx,v 1.15 2011/07/14 04:26:09 richi Exp $
//////////////////////////////////////////////////////////////////
//
// Utility class: mMutPackPRDF:
// Author: H.Pereira
// Date: 1/07/01
// Description: pack mutr hits in PRDF format. Needed for PISAtoPRDF loop
//              
//////////////////////////////////////////////////////////////////

// MUTOO headers
//
#include <MUTOO.h>
#include <MUTGEOM.h>
#include <PHException.h>
#include <PHTimer.h>
#include <MutGeom.h>
#include <MutStrip.h>
#include <PHRawDataNode.h>
#include <packet_A.h>

#include "mMutPackPRDF.h"
#include "mMutPackPRDFPar.h"

using namespace std;

//_____________________________________________________
mMutPackPRDF::mMutPackPRDF() : 
  _timer(PHTimeServer::get()->insert_new("mMutPackPRDF"))
{
  // initialize _data_node_array
  _data_node_array.assign(0);
  
  MUTOO::TRACE("initializing module mMutPackPRDF",MUTOO::ALOT);
}

//_____________________________________________________
// Event method.
PHBoolean mMutPackPRDF::event(PHCompositeNode* top_node)
{

  // Timer
  //
  _timer.get()->restart(); 
  
  try { 
    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // reset rawdata list
    _raw_packet_map.clear();

    // dump hit map
    if(_mod_par->get_verbosity() >= MUTOO::ALOT) _hit_map->print();  

    // loop over hits, pack Packet_map
    TMutHitMap::iterator hit_iter = _hit_map->range();
    while( TMutHitMap::pointer hit_ptr = hit_iter.next() )
    pack_channel_data( *hit_ptr );
      
    // convert packet map into DCM words and write to PRDF node
    fill_dcm_words( );
    write_dcm_words( );
    
    
  } catch( exception &e ) {
    cout << e.what() << endl;
    return false;
  }
  
  _timer.get()->stop(); 

  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();     
  return true;
  
}

//_____________________________________________________
/*! Reset IOC and external interface pointers */
void mMutPackPRDF::set_interface_ptrs(PHCompositeNode* top_node)
{  

  // module parameters
  _mod_par = TMutNode<mMutPackPRDFPar>::find_node(top_node,"mMutPackPRDFPar");  

  // hit map pointer
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");  
 
  // event header
  _event_header = TMutNode<headerWrapper>::find_io_node(top_node, "header");
  _header_table = _event_header->TableData();
  
  // prdf node
  PHNodeIterator iter(top_node);
  _prdf_node = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SIMPRDF"));
  if( !_prdf_node ) _prdf_node = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PRDF"));
  
  if( !_prdf_node )
  throw runtime_error( DESCRIPTION( "could not find PRDF/SIMPRDF node" ) );

} 

//_____________________________________________________
bool mMutPackPRDF::pack_channel_data( const TMutHitMap::value_type &hit )
{  

  // stores hit location into locals for clarity
  int arm( hit.get()->get_arm() );
  int station( hit.get()->get_station() );
  int octant( hit.get()->get_octant() );
  int half_octant( hit.get()->get_half_octant() );
  int gap( hit.get()->get_gap() );
  int cathode( hit.get()->get_cathode() );
  int strip( hit.get()->get_strip() );
    
  // set arm pointer    
  MutArm* arm_ptr( (arm == MUTOO::South ) ? SouthArm():NorthArm() );


  unsigned int dcm_channel( 0 ), packet_id( 0 );

  /* 
    check hit indexes validity
    this is needed since the arm_ptr access will not throw any exception
  */
  
  BOUNDS_CHECK( station, MUTOO::NumberOfStations );
  BOUNDS_CHECK( octant, MUTOO::NumberOfOctants );
  BOUNDS_CHECK( half_octant, MUTOO::NumberOfHalfOctants );
  
  int n_gaps(
    arm_ptr->f_pMutStations[station]
      ->f_pMutOctants[octant]
      ->f_pMutHalfOctants[half_octant]->getNumberOfGaps() ); 
  BOUNDS_CHECK( gap, n_gaps );

  BOUNDS_CHECK( cathode, MUTOO::NumberOfCathodePlanes );
  
  unsigned int cathode_id( ( cathode == 0 ) ? MUTGEOM::Cathode1:MUTGEOM::Cathode2 );
  int n_strips(arm_ptr->f_pMutStations[station]
    ->f_pMutOctants[octant]
    ->f_pMutHalfOctants[half_octant]
    ->f_pMutGaps[gap]
    ->f_pMutPlanes[cathode_id]->f_pMutStrips.size( ) ); 
  BOUNDS_CHECK( strip, n_strips );

  // retrieve dcm channel
  dcm_channel = arm_ptr->f_pMutStations[station]
    ->f_pMutOctants[octant]
    ->f_pMutHalfOctants[half_octant]
    ->f_pMutGaps[gap]
    ->f_pMutPlanes[cathode_id]
    ->f_pMutStrips[strip]->getDCMChannel();
  
  // retrieve packed id
  packet_id = arm_ptr->f_pMutStations[station]
    ->f_pMutOctants[octant]
    ->f_pMutHalfOctants[half_octant]
    ->f_pMutGaps[gap]
    ->f_pMutPlanes[cathode_id]
    ->f_pMutStrips[strip]->getPacket_ID();       
 
  // create a RawData object
  RawData raw_data( dcm_channel, hit );
      
  // retrieve packet  from map or create a new one, adds RawData structure to list
  _raw_packet_map[ packet_id ].add_raw_data( raw_data );

  return true;
}

//_____________________________________________________
bool mMutPackPRDF::fill_dcm_words( void )
{
  
  // loop over RawPacketMap
  for( RawPacketMap::iterator raw_packet_iter = _raw_packet_map.begin();  raw_packet_iter != _raw_packet_map.end(); raw_packet_iter++ ) 
  {
    
    RawPacket &raw_packet( raw_packet_iter->second );
    
    // check number of associated RawData objects
    if( !raw_packet.get_raw_data_list().size() ) continue;
    
    /* 
      retrieve first hit. needed to have access to octant, station and arm as well as AMU cells
      all these should be the same for all hits in the packet, though there is no consistancy check performed
    */
    const TMutHitMap::value_type& first_hit( raw_packet.get_raw_data_list().front().get_hit() );
          
    // fill DCM words    
    // DCM header information
    unsigned int n_words( 0 );
    unsigned long int high_bit( 1 );
    unsigned long int channel( 0 );
    unsigned long int running_xor( 0 );
 
    // FEM flag. assume 0
    unsigned long int flag( 0 );
    raw_packet.set_dcm_word( n_words, high_bit, channel, n_words, flag );
    running_xor ^= flag;
    n_words++;     

    // FEM module_id (ModADD)
    unsigned long int module_id =  
      MUTOO_FEM::OCSTART*first_hit.get()->get_octant() + 
      MUTOO_FEM::STSTART*first_hit.get()->get_station() + 
      MUTOO_FEM::ARMSTART*first_hit.get()->get_arm();
    raw_packet.set_dcm_word( n_words, high_bit, channel, n_words, module_id );
    running_xor ^= module_id;
    n_words++;     

    // event number
    unsigned long int event_number = static_cast<unsigned long int>( _header_table ? _header_table[0].event : -1 );
    raw_packet.set_dcm_word( n_words, high_bit, channel, n_words, event_number);
    running_xor ^= event_number;
    n_words++;     

    // beam clock (assume 0)
    unsigned long int beam_clock( 0 );
    raw_packet.set_dcm_word( n_words, high_bit, channel, n_words, beam_clock );
    running_xor ^= beam_clock;
    n_words++;     
  
    // detector ID (here, a cathode)
    unsigned long int detector_id = MUTOO_FEM::CATHDETID;
    PHDWORD det_word = raw_packet.set_dcm_word( n_words, high_bit, channel, n_words, detector_id );
    running_xor ^= detector_id;
    n_words++;     
    
    // dump detector ID word for debug
    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    cout << "mMutPackPRDF::fill_dcm_words - detector ID word=" << det_word << endl;
    
    // loop over samples to store amu cell
    n_words = MUTOO_FEM::NHWORDS;
    for( int sample = 0; sample < MUTOO_FEM::NSAMPLES; sample++ ) {
      
      // add amu cell
      raw_packet.set_dcm_word( n_words,  
        high_bit, 
        0, 
        static_cast<unsigned long int>( sample ), 
        static_cast<unsigned long int>( first_hit.get()->get_amu( sample ) ) 
      );
    
      // increment word counter
      running_xor ^= static_cast<unsigned long int>( first_hit.get()->get_amu( sample ) );
      n_words++;
    
    }
    
    // this is the sample ordering as comes out from FPGA
    static boost::array<unsigned int, 4> sample_order_FPGA = {{2,0,1,3}};

    // loop over associated RawData
    high_bit = 0;
    const RawDataList &raw_data_list( raw_packet.get_raw_data_list() );
    for( RawDataList::const_iterator raw_data_iter = raw_data_list.begin(); raw_data_iter != raw_data_list.end(); raw_data_iter++ )
    {
      // loop over samples to store adc value
      for( int sample = 0; sample < MUTOO_FEM::NSAMPLES; sample++ ) 
      {
        unsigned int word_count = sample_order_FPGA[sample];
        raw_packet.set_dcm_word( n_words,  
          high_bit, 
          raw_data_iter->get_dcm_channel(), 
          static_cast<unsigned long int>( word_count ), 
          static_cast<unsigned long int>( raw_data_iter->get_hit().get()->get_adc( word_count ) )^0x7ff
        );
    
        running_xor ^= static_cast<unsigned long int>( first_hit.get()->get_adc( word_count ) );
        
        // increment word counter
        n_words++;
        
      }
    
    }
  
    // DCM trailer information
    high_bit = 1;
    channel = 0;
    unsigned long int word_count( 0 );
    
    // user words (assume 0) 
    static boost::array<unsigned long int, MUTOO_FEM::NUWORDS> user_words = {{0,0,0,0,0,0,0, MUTOO_FEM::FPGAVERSIONRUN7}};
    

    for( int user_word_id = 0; user_word_id < MUTOO_FEM::NUWORDS; user_word_id++ ) 
    {
      
      PHDWORD dcm_word = raw_packet.set_dcm_word( n_words, high_bit, channel, MUTOO_FEM::NUWORDS, user_words[user_word_id] );
      running_xor ^= user_words[user_word_id];
      n_words++;

      if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      cout << "mMutPackPRDF::fill_dcm_words - user word[" << user_word_id << "] = 0x" << hex << dcm_word << endl;

    }
    
    // parity (is running_xor)
    unsigned long int parity( running_xor );      
    word_count = static_cast< unsigned int long >( MUTOO_FEM::NUWORDS );
    raw_packet.set_dcm_word( n_words, high_bit, channel, word_count, parity);
    n_words++;
    
    // CAV2 (?) (assume 0)
    channel = 2047;
    word_count = 15;
    unsigned long int CAV2( 0 );      
    raw_packet.set_dcm_word( n_words, high_bit, channel, word_count, CAV2);
    n_words++;
  
  }

  return true;
  
}
  
//_____________________________________________________
bool mMutPackPRDF::write_dcm_words( void )
{ 
  
  if( _mod_par->get_verbosity() >= MUTOO::MAX )
  MUTOO::PRINT( cout, "mMutPackPRDF::write_dcm_words" );

  // define mut tag
  string base( "mutPRDF000" );  // assumes maximum of 999 rows
  static const int bytes_per_word( 4 );
  static const int hit_format( IDMUTC_FPGA0SUP );
   
  // loop over packet map
  unsigned int n_dcm_rows( 0 );
  for( RawPacketMap::const_iterator raw_packet_iter = _raw_packet_map.begin();  raw_packet_iter != _raw_packet_map.end(); raw_packet_iter++ ) 
  {
    
    // check number of recorded DCM raws
    if( n_dcm_rows >= MAX_DCM_ROWS ) 
    throw runtime_error( DESCRIPTION( "mMutPackPRDF::write_dcm_words - reach limit on max DCM rows" ) );
         
    string encoded( encode_string(base, n_dcm_rows) );
    PHDWORD *dcm_words_ptr = (PHDWORD*) raw_packet_iter->second.get_dcm_words_address();
  
    // check if _data_node_array is big enough
    if( !_data_node_array[n_dcm_rows] ) {
      
      // create a new node
      _data_node_array[n_dcm_rows] = new PHRawDataNode(
        dcm_words_ptr, 
        encoded.c_str(), 
        raw_packet_iter->second.get_n_words(), raw_packet_iter->first, bytes_per_word, hit_format
      );
      
      // adds to PRDF node
      _prdf_node->addNode( _data_node_array[n_dcm_rows] );
    
    } else {
    
      // update existing node
      PHRawDataNode *raw_data_ptr = _data_node_array[n_dcm_rows];
      raw_data_ptr->setData(dcm_words_ptr);
      raw_data_ptr->setLength(raw_packet_iter->second.get_n_words() );
      raw_data_ptr->setID(raw_packet_iter->first);
      raw_data_ptr->setWordLength(bytes_per_word);
      raw_data_ptr->setHitFormat(hit_format);
    
    }
  
    if( _mod_par->get_verbosity() >= MUTOO::MAX )
    cout << "mMutPackPRDF::write_dcm_words - wrote " << raw_packet_iter->second.get_n_words()  
      << " words to packet " << raw_packet_iter->first
      << " (" << encoded << ").\n";
         
    n_dcm_rows++;
  }
  
  if( _mod_par->get_verbosity() >= MUTOO::MAX )
  cout << "mMutPackPRDF::write_dcm_words - wrote " << n_dcm_rows << " non empty rows.\n";
  
  // reset remaining nodes if any
  for( unsigned int raw = n_dcm_rows; raw < MAX_DCM_ROWS && _data_node_array[raw]; raw++ ) {
    _data_node_array[raw]->setLength(0);
    _data_node_array[raw]->setID(0);
  }
  
  if( _mod_par->get_verbosity() >= MUTOO::MAX )
  MUTOO::PRINT( cout, "**" );
  
  return true;
}
  
//________________________________________________
string mMutPackPRDF::encode_string( const string& base, unsigned int row_id ) 
{
  
  // check base size
  if( base.size() != 10 ) 
  throw runtime_error( DESCRIPTION( "wrong size for base string" ) );
  
  // output string
  string out( base );
  
  // check row is between 0 and 999
  assert( row_id < 1000 );

  Int_t hundred = row_id/100;
  Int_t ten = (row_id % 100 )/10;;
  Int_t unit = (row_id % 10 );
  out[9] = 48 + unit;
  out[8] = 48 + ten;
  out[7] = 48 + hundred;
  return out;
  
}
