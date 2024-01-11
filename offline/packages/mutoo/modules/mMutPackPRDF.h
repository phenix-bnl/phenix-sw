// $Id: mMutPackPRDF.h,v 1.5 2006/03/16 21:02:57 hpereira Exp $
#ifndef __mMutPackPRDF_HH__
#define __mMutPackPRDF_HH__

#include <MUTOO_FEM.h>
#include <boost/array.hpp>
#include <TMutHitMap.h>
#include <PHTimeServer.h>
#include <memory>
#include <headerWrapper.h>
#include <packet_A.h>

class PHRawDataNode;
class mMutPackPRDFPar;
class PHCompositeNode;

/*! \ingroup modules */
//! picks hit map IOC convert to raw data and write to PRDF
/*! 
  retrieve MUTR hit map, build raw data ADC samples, writes to PRDF.
  the module must run after the response module mMutResponse, from MC hits
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutPackPRDFPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutHitMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
</table>
*/
class mMutPackPRDF
{
 public: 

  //! constructor
  mMutPackPRDF(); 
  
  //! destructor
  virtual ~mMutPackPRDF() {}; 
  
  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  

  // private methods

  //! retrieves parameter node and IOC
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! create channel data, returns true if added
  bool pack_channel_data( const TMutHitMap::value_type& hit);
 
  //! create string from base + row ID
  static std::string encode_string( const std::string& base, unsigned int raw_id );
 
  //! fill RawPacket DCM words corresponding to associated hits
  bool fill_dcm_words( void );
 
  //! write dcm words to PRDF node
  bool write_dcm_words( void );

  enum{ MAX_DCM_WORDS = 532, MAX_DCM_ROWS=510 };

  // Interface pointers
  TMutHitMap* _hit_map;			    //!< hit map IOC
  headerWrapper* _event_header; //!< event header external interface (from pisa)
  HEADER_ST* _header_table;     //!< event header table (from pisa)
  PHCompositeNode* _prdf_node;  //!< prdf node

  //! module parameter module
  mMutPackPRDFPar* _mod_par;
  
  // raw data array to be written to PRDF node
  boost::array<PHRawDataNode*, MAX_DCM_ROWS> _data_node_array;

  // Timer
  PHTimeServer::timer _timer;
  
  //! raw data
  class RawData
  {
    public:
    //! conststuctor
    RawData( const int& dcm_channel, const TMutHitMap::value_type& hit ):
      _dcm_channel( dcm_channel ),
      _hit( hit )
    {}
    
    //! retrieve dcm_channel
    unsigned int get_dcm_channel( void ) const
    { return _dcm_channel; }
    
    //! retrieve associated TMutHit
    TMutHitMap::value_type get_hit( void ) const
    { return _hit; }
      
    private:
    unsigned int _dcm_channel;    //!< dcm_channel
    TMutHitMap::value_type _hit;  //! hit
    
  };
  
  // shortcut for RawData list
  typedef std::list<RawData> RawDataList;
    
  //! packed data
  class RawPacket
  {
  
    public:
    
    //! constructor
    RawPacket( void ):
      _n_words( 0 )
    { _dcm_words.assign(0); }
    
    //! adds a new data to internal list  
    void add_raw_data( const RawData& data )
    { _raw_data_list.push_back( data ); }
    
    //! retrieves packet list
    const RawDataList& get_raw_data_list( void ) const
    { return _raw_data_list; }
    
    // defines DCM word
    unsigned long int set_dcm_word( int word_id, 
      unsigned long int high_bit, 
      unsigned long int dcm_channel, 
      unsigned long int word_count, 
      unsigned long int data )
    { 
      
      // check word_id is valid
      BOUNDS_CHECK( word_id, MAX_DCM_WORDS );
      
      // check word_id against total number of valid words
      if( word_id+1 > _n_words ) _n_words = word_id + 1;     
      
      if( _dcm_words[word_id] )
      std::cerr << "mMutUnpack::RawData::set_word - overwriting existing word " << word_id << ".\n";
        
      unsigned long int word = 0x0;
      word |= (high_bit*   0x08000000);
      word |= (dcm_channel*0x00100000);
      word |= (word_count* 0x00010000);
      word |= (data&       0x0000FFFF);       
      
      _dcm_words[word_id] = word;
      return word;
      
    }
    
    //! returns reference to list of dcm words  
    boost::array< unsigned long int, MAX_DCM_WORDS >& get_dcm_words( void )
    { return _dcm_words; }
    
    //! returns address of first element in dcm word list
    const unsigned long int* get_dcm_words_address( void ) const
    { return &_dcm_words[0]; }
    
    //! return number of valid words stored (including trailer/header
    int get_n_words( void ) const
    { return _n_words; }
    
    private:
    
    //! list of DCM words associated to this hit
    boost::array< unsigned long int, MAX_DCM_WORDS > _dcm_words;    
    
    /*! 
      \brief
      number of recorded valid words
      this is actualy misleading. This is 1+largest index of recorded word.
    */
    int _n_words;
    
    //! list of packets hit structure for given packet id    
    RawDataList _raw_data_list; 
    
  };
  
  //! list of all packet ids
  typedef std::map< unsigned int, RawPacket > RawPacketMap;
  RawPacketMap _raw_packet_map;

};

#endif /* __MMUTUNPACK_HH__ */

