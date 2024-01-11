// $Id: mFvtxPackPRDF.cxx,v 1.1 2011/04/12 18:23:32 youzy Exp $
//////////////////////////////////////////////////////////////////
//
// Utility class: mFvtxPackPRDF:
// Author: Zhengyun You
// Date: 02/09/11
// Description: pack fvtx hits in PRDF format. Needed for PISAtoPRDF loop
//              
//////////////////////////////////////////////////////////////////

// FVTXOO headers
//
#include <FVTXOO.h>
#include <FVTXGEOM.h>
#include <PHException.h>
#include <PHTimer.h>
#include <FvtxGeom.h>
#include <FvtxStrip.h>
#include <PHRawDataNode.h>
#include <packet_A.h>

#include "mFvtxPackPRDF.h"
#include "mFvtxPackPRDFPar.h"

using namespace std;

//_____________________________________________________
mFvtxPackPRDF::mFvtxPackPRDF() : 
  _timer(PHTimeServer::get()->insert_new("mFvtxPackPRDF"))
{
  // initialize _data_node_array
  _data_node_array.assign(0);

  FVTXOO::TRACE("initializing module mFvtxPackPRDF");
}

//_____________________________________________________
// Event method.
PHBoolean mFvtxPackPRDF::event(PHCompositeNode* top_node)
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
    if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _hit_map->print();  

    // loop over hits, pack Packet_map
    TFvtxHitMap::iterator hit_iter = _hit_map->range();
    while( TFvtxHitMap::pointer hit_ptr = hit_iter.next() )
    pack_channel_data( *hit_ptr );
      
    // convert packet map into DCM words and write to PRDF node
    fill_dcm_words( );
    write_dcm_words( );
    
    
  } catch( exception &e ) {
    cout << e.what() << endl;
    return false;
  }
  
  _timer.get()->stop(); 

  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();     
  return true;
  
}

//_____________________________________________________
/*! Reset IOC and external interface pointers */
void mFvtxPackPRDF::set_interface_ptrs(PHCompositeNode* top_node)
{  

  // module parameters
  _mod_par = TMutNode<mFvtxPackPRDFPar>::find_node(top_node,"mFvtxPackPRDFPar");  

  // hit map pointer
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");  
 
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
bool mFvtxPackPRDF::pack_channel_data( const TFvtxHitMap::value_type &hit )
{  

  // stores hit location into locals for clarity
  int arm( hit.get()->get_arm() );
  int cage( hit.get()->get_cage() );
  int station( hit.get()->get_station() );
  int sector( hit.get()->get_sector() );
  int column( hit.get()->get_column() );
  int strip( hit.get()->get_strip() );
    
  // set arm pointer    
  FvtxArm* arm_ptr( (arm == FVTXOO::South ) ? FvtxGeom::south_arm() : FvtxGeom::north_arm() );


  unsigned int dcm_channel( 0 ), packet_id( 0 ), fem_id( 0 ), chip_id( 0 );

  /* 
    check hit indexes validity
    this is needed since the arm_ptr access will not throw any exception
  */
  
  BOUNDS_CHECK( cage, FVTXGEOM::NumberOfCages );
  BOUNDS_CHECK( station, FVTXGEOM::NumberOfStations );
  BOUNDS_CHECK( sector, FVTXGEOM::NumberOfSectors );
  BOUNDS_CHECK( column, FVTXGEOM::NumberOfColumns );
  
  int n_strips( arm_ptr
    ->get_cage( cage )
    ->get_station( station )
    ->get_sector( sector )
    ->get_column( column )
    ->get_n_strips() ); 
  BOUNDS_CHECK( strip, n_strips );

  // retrieve dcm channel
  dcm_channel = arm_ptr
    ->get_cage( cage )
    ->get_station( station )
    ->get_sector( sector )
    ->get_column( column )
    ->get_strip( strip )
    ->get_dcm_channel();

  // retrieve chip id
  chip_id = arm_ptr
    ->get_cage( cage )
    ->get_station( station )
    ->get_sector( sector )
    ->get_column( column )
    ->get_strip( strip )
    ->get_chip_id();

  // retrieve fem id
  fem_id = arm_ptr
    ->get_cage( cage )
    ->get_station( station )
    ->get_sector( sector )
    ->get_column( column )
    ->get_strip( strip )
    ->get_fem_id();

  // retrieve packed id
  packet_id = arm_ptr
    ->get_cage( cage )
    ->get_station( station )
    ->get_sector( sector )
    ->get_column( column )
    ->get_strip( strip )
    ->get_packet_id();

  if ( _mod_par->get_verbosity() >= FVTXOO::MAX )
  {
    cout << "arm " << arm << " cage " << cage << " station " << station << " sector " << sector << " column " << column << " strip " << strip 
       << " packet " << packet_id << " fem " << fem_id << " chip " << chip_id << " channel " << dcm_channel << endl;
  }

  // create a RawData object
  RawData raw_data( fem_id, chip_id, dcm_channel, hit );
      
  // retrieve packet  from map or create a new one, adds RawData structure to list
  _raw_packet_map[ packet_id ].add_raw_data( raw_data );

  return true;
}

//_____________________________________________________
bool mFvtxPackPRDF::fill_dcm_words( void )
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
    const TFvtxHitMap::value_type& first_hit( raw_packet.get_raw_data_list().front().get_hit() );
          
    // fill DCM words    
    // DCM header information
    unsigned int n_words( 0 );
    unsigned long int high_bit( 0x4004 );
    unsigned long int running_xor( 0 );
 
    // event number
    unsigned long int event_number = static_cast<unsigned long int>( _header_table ? _header_table[0].event : -1 );
    raw_packet.set_dcm_word( n_words, high_bit, event_number);
    running_xor ^= event_number;
    n_words++;

    high_bit = 0x0004;
    // FEM flag. assume 0x5555
    unsigned long int flag( 21845 );
    raw_packet.set_dcm_word( n_words, high_bit, flag );
    running_xor ^= flag;
    n_words++;     

    // detector ID 
    unsigned long int detector_id = FVTXOO_FEM::FVTXDETID;
    if ( _mod_par->get_verbosity() >= FVTXOO::ALOT ) cout << "det ID " << detector_id << endl;
    PHDWORD det_word = raw_packet.set_dcm_word( n_words, high_bit, detector_id );
    running_xor ^= detector_id;
    n_words++;

    // dump detector ID word for debug
    if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
    cout << "mFvtxPackPRDF::fill_dcm_words - detector ID word=" << det_word << endl;

    // FEM module_id (ModADD)
    unsigned long int module_id =  
      FVTXOO_FEM::WEDGESTART*first_hit.get()->get_sector() + 
      FVTXOO_FEM::STSTART*first_hit.get()->get_station() + 
      FVTXOO_FEM::CAGESTART*first_hit.get()->get_cage() +
      FVTXOO_FEM::ARMSTART*first_hit.get()->get_arm();
    module_id = 0xfc00;
    raw_packet.set_dcm_word( n_words, high_bit, module_id );
    running_xor ^= module_id;
    n_words++;     

    // beam clock (assume 0)
    unsigned long int beam_clock( 0 );
    raw_packet.set_dcm_word( n_words, high_bit, beam_clock );
    running_xor ^= beam_clock;
    n_words++;     
 
    RawDataList &raw_data_list( raw_packet.get_raw_data_list() );

    // sort raw data by fem_id
    if( _mod_par->get_verbosity() >= FVTXOO::MAX )
    {
      cout << "sort raw data by fem_id, before sort ";
      for( RawDataList::const_iterator raw_data_iter = raw_data_list.begin(); raw_data_iter != raw_data_list.end(); raw_data_iter++ )
      {
        cout << raw_data_iter->get_fem_id();
      }
    }

    raw_data_list.sort();

    if( _mod_par->get_verbosity() >= FVTXOO::MAX )
    {
      cout << endl << "after sort ";
      for( RawDataList::const_iterator raw_data_iter = raw_data_list.begin(); raw_data_iter != raw_data_list.end(); raw_data_iter++ )
      {
        cout << raw_data_iter->get_fem_id();
      }
      cout << endl;
    }

    unsigned int fem_id = 99;
    // loop over associated RawData
    for( RawDataList::const_iterator raw_data_iter = raw_data_list.begin(); raw_data_iter != raw_data_list.end(); raw_data_iter++ )
    {
      // start a FEM_ID word when fem_id of next hit is different.
      if (raw_data_iter->get_fem_id() != fem_id) {
        unsigned long int fem_id_word = raw_data_iter->get_fem_id();
        fem_id_word *= 0x1000;
        raw_packet.set_dcm_word( n_words, high_bit, fem_id_word );
        running_xor ^= fem_id_word;
        n_words++;
      }
      fem_id = raw_data_iter->get_fem_id();

      PHDWORD data_word = raw_packet.set_dcm_word_data(n_words,
        high_bit, 
        raw_data_iter->get_dcm_channel(),
        raw_data_iter->get_chip_id(),
        raw_data_iter->get_hit().get()->get_adc() );
      unsigned long int data = data_word ^ 0x0000FFFF;
      running_xor ^= data;
        
      // increment word counter
      n_words++;
    }
  
    // DCM trailer information
    unsigned long int err_word = 0;
    raw_packet.set_dcm_word( n_words, high_bit, err_word ); 
    n_words++;

    // parity (is running_xor)
    unsigned long int parity( running_xor );      
    high_bit = 0x8004;
    raw_packet.set_dcm_word( n_words, high_bit, parity);
    n_words++;
    
  }

  return true;
  
}
  
//_____________________________________________________
bool mFvtxPackPRDF::write_dcm_words( void )
{ 
  
  if( _mod_par->get_verbosity() >= FVTXOO::ALOT )  FVTXOO::PRINT( cout, "mFvtxPackPRDF::write_dcm_words" );

  // define mut tag
  string base( "mutPRDF000" );  // assumes maximum of 999 rows
  static const int bytes_per_word( 4 );
  //static const int hit_format( IDPXL_DCM0 );
  static const int hit_format( IDMUTC_DCM0 );
   
  // loop over packet map
  unsigned int n_dcm_rows( 0 );
  for( RawPacketMap::const_iterator raw_packet_iter = _raw_packet_map.begin();  raw_packet_iter != _raw_packet_map.end(); raw_packet_iter++ ) 
  {
    
    // check number of recorded DCM raws
    if( n_dcm_rows >= MAX_DCM_ROWS ) 
    throw runtime_error( DESCRIPTION( "mFvtxPackPRDF::write_dcm_words - reach limit on max DCM rows" ) );
         
    string encoded( encode_string(base, n_dcm_rows) );
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) cout << "encoded " << encoded << endl;
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
  
    if( _mod_par->get_verbosity() >= FVTXOO::MAX )
    {
      cout << "mFvtxPackPRDF::write_dcm_words - wrote " << raw_packet_iter->second.get_n_words()  
        << " words to packet " << raw_packet_iter->first
        << " (" << encoded << ").\n";
    }
     
    n_dcm_rows++;
  }
  
  if( _mod_par->get_verbosity() >= FVTXOO::MAX ) cout << "mFvtxPackPRDF::write_dcm_words - wrote " << n_dcm_rows << " non empty rows.\n";
  
  // reset remaining nodes if any
  for( unsigned int raw = n_dcm_rows; raw < MAX_DCM_ROWS && _data_node_array[raw]; raw++ ) {
    _data_node_array[raw]->setLength(0);
    _data_node_array[raw]->setID(0);
  }
  
  if( _mod_par->get_verbosity() >= FVTXOO::ALOT )  FVTXOO::PRINT( cout, "**" );
  
  return true;
}
  
//________________________________________________
string mFvtxPackPRDF::encode_string( const string& base, unsigned int row_id ) 
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
