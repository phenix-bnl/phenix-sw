// $Id: mMuiPackPRDF.cxx,v 1.3 2016/04/20 14:02:05 pinkenbu Exp $
//////////////////////////////////////////////////////////////////
//
// Utility class: mMuiPackPRDF:
// Author: hpereira
// Date: 1/07/01
// Description: Unpacks MUTR raw data
//              
//////////////////////////////////////////////////////////////////

// MUIOO headers
//
#include "mMuiPackPRDF.h"
#include "mMuiPackPRDFPar.h"
#include "TMuiHitMapO.h"
//INCLUDECHECKER: Removed this line: #include <PHException.h>
#include <MUIOO.h>
//INCLUDECHECKER: Removed this line: #include <PHTimer.h>
#include <TMuiAddressTable.hh>

// PHENIX headers
//
//INCLUDECHECKER: Removed this line: #include <MutGeom.h>
#include <PHRawDataNode.h>
#include <packet_A.h>

using namespace std;

/*! \ingroup modules */

//_____________________________________________________
mMuiPackPRDF::mMuiPackPRDF() : 
  _timer(PHTimeServer::get()->insert_new("mMuiPackPRDF"))
{
  // initialize _data_node_array
  _data_node_array.assign( 0 );
  
  MUIOO::TRACE("initializing module mMuiPackPRDF");  
}

//_____________________________________________________
// Event method.
PHBoolean mMuiPackPRDF::event(PHCompositeNode* top_node)
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
    if(_mod_par->get_verbosity() >= MUIOO::ALOT) _hit_map->print();  

    // loop over hits, pack Packet_map
    TMuiHitMapO::iterator hit_iter = _hit_map->range();
    while( TMuiHitMapO::pointer hit_ptr = hit_iter.next() )
    pack_channel_data( *hit_ptr );
      
    // convert packet map into DCM words and write to PRDF node
    fill_dcm_words( );
    write_dcm_words( );
    
  } catch( exception &e ) {
    cout << e.what() << endl;
    return false;
  }
  
  _timer.get()->stop(); 

  if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();     
  return true;
  
}

//_____________________________________________________
/*! Reset IOC and external interface pointers */
void mMuiPackPRDF::set_interface_ptrs(PHCompositeNode* top_node)
{  

  // module parameters
  _mod_par = TMutNode<mMuiPackPRDFPar>::find_node(top_node,"mMuiPackPRDFPar");  

  // hit map pointer
  _hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");  
 
  // event header
  _event_header = TMutNode<headerWrapper>::find_io_node(top_node, "header");
  _header_table = _event_header->TableData();
  
  // prdf node. Try load either SIMPRDF or PRDF node
  PHNodeIterator iter(top_node);
  _prdf_node = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SIMPRDF"));
  if( !_prdf_node ) _prdf_node = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PRDF"));
  
  if( !_prdf_node )
  throw runtime_error( DESCRIPTION( "could not find PRDF/SIMPRDF node" ) );
} 

//_____________________________________________________
bool mMuiPackPRDF::pack_channel_data( const TMuiHitMapO::value_type &hit )
{  

  // stores hit location into locals for clarity
  int arm( hit.get()->get_arm() );
  int plane( hit.get()->get_plane() );
  int panel( hit.get()->get_panel() );
  int twopack( hit.get()->get_twopack() );
  EOrient_t orientation( hit.get()->get_orientation() ? kVERT:kHORIZ );

  TMuiChannelId id_soft;
  TMuiReadoutID id_hard;
  
  // retrieve soft/hard address associated to hit
  id_soft.Set( arm, plane, panel, orientation, twopack );
  id_hard = TMuiAddressTable::Table()->HardwareAddress(id_soft);
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
    cout << "mMuiPackPRDF::pack_channel_data - software ID " << id_soft << endl;
    cout << "mMuiPackPRDF::pack_channel_data - hardware ID " << id_hard << endl;
  }
  
  short fem( id_hard.FEM() );
  if( fem < 0 || fem >= MUIOO::kFEMsTotal )
  throw runtime_error( DESCRIPTION( "invalid fem" ) );
 
  // create a RawData object
  RawData raw_data( id_soft, id_hard, hit );
      
  // retrieve packet  from map or create a new one, adds RawData structure to list
  RawPacket &raw_packet( _raw_packet_map[ fem ] );
  raw_packet.add_raw_data( raw_data );

  // build packet_id if not set
  if( !raw_packet.get_packet_id() ) {

    // Fill in DCM header words.
    static const unsigned long int packet_id_base = 12001;
    unsigned long int packet_id( packet_id_base );
    if ( arm == kNORTH) packet_id += 2;
    if ( orientation == kVERT) packet_id++;
    
    raw_packet.set_packet_id( packet_id );

  }
    

  return true;
}

//_____________________________________________________
bool mMuiPackPRDF::fill_dcm_words( void )
{
  
  static const unsigned long int header_bit  = 1 << 31;
  static const unsigned long int user_bit    = header_bit | (1 << 24);
  static const unsigned long int trailer_bit = (0xFFFC) << 16;
  static const unsigned long int roc_offset   = 24;
  static const unsigned long int word_offset  = 20;
  
  // static detector IDs
  boost::array<unsigned long int, 4 > detector_id = {{ 0x0C00, 0x0C00, 0x0C01, 0x0C01 }};
  boost::array<unsigned long int, 4 > module_id = {{ 0x0000, 0x0010, 0x1000, 0x1010 }};
  
  // loop over RawPacketMap
  for( RawPacketMap::iterator raw_packet_iter = _raw_packet_map.begin();  raw_packet_iter != _raw_packet_map.end(); raw_packet_iter++ ) 
  {
    
    unsigned long int running_xor( 0 );
    
    RawPacket &raw_packet( raw_packet_iter->second );
    
    // check number of associated RawData objects
    if( !raw_packet.get_raw_data_list().size() ) continue;
    
    // fill header words
    int n_words( 0 );
    
    // detector id
    raw_packet.set_dcm_word( n_words, header_bit | (n_words << word_offset) | detector_id[ raw_packet_iter->first ] );
    running_xor ^= detector_id[ raw_packet_iter->first ];
    n_words++;     

    // event number
    unsigned long int event_number = static_cast<unsigned long int>( _header_table ? _header_table[0].event : -1 );
    raw_packet.set_dcm_word( n_words, header_bit | (n_words << word_offset) | event_number );
    running_xor ^= event_number;
    n_words++;     
 
    // module id
    raw_packet.set_dcm_word( n_words, header_bit | (n_words << word_offset) | module_id[ raw_packet_iter->first ] );
    running_xor ^= module_id[ raw_packet_iter->first ];
    n_words++;     

    // FEM flag. assume 0
    unsigned long int flag( 0 );
    raw_packet.set_dcm_word( n_words, header_bit | (n_words << word_offset) | flag );
    running_xor ^= flag;
    n_words++;     

    // beam clock (assume 0)
    unsigned long int beam_clock( 0 );
    raw_packet.set_dcm_word( n_words, header_bit | (n_words << word_offset) | beam_clock );
    running_xor ^= beam_clock;
    n_words++;     

    // loop over associated RawData, initialize data_words
    const RawDataList &raw_data_list( raw_packet.get_raw_data_list() );
    for( RawDataList::const_iterator raw_data_iter = raw_data_list.begin(); raw_data_iter != raw_data_list.end(); raw_data_iter++ )
    {
      
      // retrieve roc id and word
      const TMuiReadoutID & id( raw_data_iter->get_id_hard() );
      short word_index = id.WordIndex() - 1;
      if ( (word_index < 0) || (word_index >= MUIOO::kWordsPerFEM) )
      throw runtime_error( DESCRIPTION( "invalid word_index" ) );
      
	    raw_packet.get_data_word( word_index ) |= id.BitPattern();
    
    }     
      
    // loop over data words fill dcm_words
    for( int data_word=0; data_word<MUIOO::kWordsPerFEM; data_word++ )
    {
 
      // retrieve roc id and word
      TMuiReadoutID id = TMuiAddressTable::Table()->HardwareAddress(module_id[ raw_packet_iter->first ], data_word + 1);
      unsigned long int roc   = id.ROC();
      unsigned long int rword = id.Word();
      
      // unconnected channels
      if( (id.ROC()==-1 ) && ( id.Word()==-1 ) && ( id.Channel()==-1 ) ) {
        
        if( raw_packet.get_data_word( data_word ) ) 
        cout << "mMuiPackPRDF::fill_dcm_word - warning not connected channel have data.\n";
        
        continue;
      
      }
      
      // check roc id 
      if( roc >= (unsigned long int)MUIOO::kROCsPerFEM )
      throw runtime_error( DESCRIPTION( "invalid roc id" ) );

      //      if ( (rword < 0) || (rword >= (unsigned int)MUIOO::kWordsPerROC) )
      // rword is unsigned so checking < 0 is useless
      if ( rword >= (unsigned int)MUIOO::kWordsPerROC )
      throw runtime_error( DESCRIPTION( "invalid rword" ) );
      
      // set data word
      raw_packet.set_dcm_word( n_words, raw_packet.get_data_word( data_word ) | ((roc) << roc_offset) | ((rword) << word_offset) );
      running_xor ^= raw_packet.get_data_word( data_word );
      n_words++;
          
    }
        
    // DCM trailer information
    static const int n_user_words( 8 );
    
    // user words (assume 0)
    for( int user_word = 0; user_word < n_user_words; user_word++ ) {
      unsigned long int word_count = static_cast< unsigned int long >( user_word );
      unsigned long int word = 0;
      raw_packet.set_dcm_word( n_words, user_bit | ( word_count << word_offset ) | word );
      running_xor ^= word;
      n_words++;
    }
    
    // parity (is running_xor)
    unsigned long int parity( running_xor );      
    raw_packet.set_dcm_word( n_words, header_bit | parity );
    n_words++;
     
    // CAV2 (?) (assume 0)
    unsigned long int CAV2( 0 );      
    raw_packet.set_dcm_word( n_words, trailer_bit | CAV2 );
    n_words++;
  
  }

  return true;
  
}
  
//_____________________________________________________
bool mMuiPackPRDF::write_dcm_words( void )
{ 
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  MUIOO::PRINT( cout, "mMuiPackPRDF::write_dcm_words" );

  // define mut tag
  string base( "muiPRDF000" );  // assumes maximum of 999 rows
  static const int bytes_per_word( 4 );
  static const int hit_format( IDMUID_FPGA0SUP );
   
  // loop over packet map
  int n_dcm_rows( 0 );
  for( RawPacketMap::const_iterator raw_packet_iter = _raw_packet_map.begin();  raw_packet_iter != _raw_packet_map.end(); raw_packet_iter++ ) 
  {
    
    // check number of recorded DCM raws
    if( n_dcm_rows >= MUIOO::kFEMsTotal ) 
    throw runtime_error( DESCRIPTION( "mMuiPackPRDF::write_dcm_words - reach limit on max DCM rows" ) );
             
    string encoded( encode_string(base, n_dcm_rows) );
    PHDWORD *dcm_words_ptr = (PHDWORD*) raw_packet_iter->second.get_dcm_words_address();
  
    // check if _data_node_array is big enough
    if( !_data_node_array[n_dcm_rows] ) {
      
      // create a new node
      _data_node_array[n_dcm_rows] = new PHRawDataNode(
        dcm_words_ptr, 
        encoded.c_str(), 
        raw_packet_iter->second.get_n_words(), raw_packet_iter->second.get_packet_id(), bytes_per_word, hit_format
      );
      
      // adds to PRDF node
      _prdf_node->addNode( _data_node_array[n_dcm_rows] );
    
    } else {
    
      // update existing node
      PHRawDataNode *raw_data_ptr = _data_node_array[n_dcm_rows];
      raw_data_ptr->setData(dcm_words_ptr);
      raw_data_ptr->setLength(raw_packet_iter->second.get_n_words() );
      raw_data_ptr->setID(raw_packet_iter->second.get_packet_id());
      raw_data_ptr->setWordLength(bytes_per_word);
      raw_data_ptr->setHitFormat(hit_format);
    
    }
  
    if( _mod_par->get_verbosity() >= MUIOO::ALOT )
    cout << "mMuiPackPRDF::write_dcm_words - wrote " << raw_packet_iter->second.get_n_words()  
      << " words to packet " << raw_packet_iter->first
      << " (" << encoded << ").\n";
         
    n_dcm_rows++;
      
  }
  
  if( _mod_par->get_verbosity() >= MUIOO::ALOT )
  cout << "mMuiPackPRDF::write_dcm_words - wrote " << n_dcm_rows << " non empty rows.\n";
  
  // reset remaining nodes if any
  for( int raw = n_dcm_rows; raw < MUIOO::kFEMsTotal && _data_node_array[raw]; raw++ ) {
    _data_node_array[raw]->setLength(0);
    _data_node_array[raw]->setID(0);
  }
  
  if( _mod_par->get_verbosity() >= MUIOO::MAX )
  MUIOO::PRINT( cout, "**" );
  
  return true;
}
  
//________________________________________________
string mMuiPackPRDF::encode_string( const string& base, unsigned int row_id ) 
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
