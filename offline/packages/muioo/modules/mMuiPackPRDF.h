// $Id: mMuiPackPRDF.h,v 1.1 2006/04/22 01:59:14 hpereira Exp $
#ifndef __mMuiPackPRDF_HH__
#define __mMuiPackPRDF_HH__

#include <boost/array.hpp>
#include <boost/shared_ptr.hpp>
#include <TMuiHitMapO.h>
#include <TMuiChannelId.hh>
#include <TMuiReadoutID.hh>
#include <PHTimeServer.h>
#include <MUIOO.h>
#include <memory>
#include <headerWrapper.h>

class MutDCMChannelMap;
class PHRawDataNode;
class mMuiPackPRDFPar;

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
<td> const mMuiPackPRDFPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMuiHitMapO*</td>
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
class mMuiPackPRDF
{
 public: 

  //! constructor
  mMuiPackPRDF(); 
  
  //! destructor
  virtual ~mMuiPackPRDF() {} 
  
  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  

  // private methods

  //! retrieves parameter node and IOC
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! create channel data, returns true if added
  bool pack_channel_data( const TMuiHitMapO::value_type& hit);
 
  //! create string from base + row ID
  static std::string encode_string( const std::string& base, unsigned int raw_id );
 
  //! fill RawPacket DCM words corresponding to associated hits
  bool fill_dcm_words( void );
 
  //! write dcm words to PRDF node
  bool write_dcm_words( void );

  // Interface pointers
  TMuiHitMapO* _hit_map;			  //!< hit map IOC
  headerWrapper* _event_header; //!< event header external interface (from pisa)
  HEADER_ST* _header_table;     //!< event header table (from pisa)
  PHCompositeNode* _prdf_node;  //!< prdf node

  //! module parameter module
  mMuiPackPRDFPar* _mod_par;
  
  // raw data array to be written to PRDF node
  boost::array< PHRawDataNode*, MUIOO::kFEMsTotal> _data_node_array;

  // Timer
  PHTimeServer::timer _timer;
  
  //! raw data
  class RawData
  {
    public:
    //! conststuctor
    RawData( 
      const TMuiChannelId& id_soft,
      const TMuiReadoutID& id_hard,
      const TMuiHitMapO::value_type& hit 
    ):
      _id_soft( id_soft ),
      _id_hard( id_hard ),
      _hit( hit )
    {}
    
    //! retrieve software index
    TMuiChannelId get_id_soft( void ) const
    { return _id_soft; }
    
    //! retrieve hardware index
    TMuiReadoutID get_id_hard( void ) const
    { return _id_hard; }
    
    //! retrieve associated TMuiHit
    TMuiHitMapO::value_type get_hit( void ) const
    { return _hit; }
      
    private:
    TMuiChannelId _id_soft; //! soft index associated to channel
    TMuiReadoutID _id_hard; //! hard index associated to channel
    TMuiHitMapO::value_type _hit;  //! hit
    
  };
  
  // shortcut for RawData list
  typedef std::list<RawData> RawDataList;
  
  // number of header/trailer words
  enum { NHWORDS =5, NTWORDS=10 };
  enum { NTOTWORDS= MUIOO::kWordsPerFEM+NHWORDS+NTWORDS };
  //! packed data
  class RawPacket
  {
  
    public:
    
    //! constructor
    RawPacket( void ):
      _packet_id( 0 ),
      _n_words( 0 )
    { 
    
      _data_words.assign( 0 );
      _dcm_words.assign( 0 );
        
    }
    
    //! sets packet ID
    void set_packet_id( unsigned long int id )
    { _packet_id = id; }
    
    //! retrieves packet ID
    unsigned long int get_packet_id( void ) const
    { return _packet_id; }    
    
    //! adds a new data to internal list  
    void add_raw_data( const RawData& data )
    { _raw_data_list.push_back( data ); }
    
    //! retrieves packet list
    const RawDataList& get_raw_data_list( void ) const
    { return _raw_data_list; }
    
    //! retrieve data word
    unsigned long int& get_data_word( int word_id )
    { 
      BOUNDS_CHECK( word_id, MUIOO::kWordsPerFEM );
      return _data_words[word_id];
    }
    
    //! retrieve data word
    unsigned long int get_data_word( int word_id ) const
    { 
      BOUNDS_CHECK( word_id, MUIOO::kWordsPerFEM );
      return _data_words[word_id];
    }
      
    //! defines DCM word
    void set_dcm_word( 
      int word_id, 
      unsigned long int value  
    )
    { 
      
      // check word_id is valid
      BOUNDS_CHECK( word_id, NTOTWORDS );
      
      // check word_id against total number of valid words
      if( word_id+1 > _n_words ) _n_words = word_id + 1;     
      
      if( _dcm_words[word_id] )
      std::cerr << "mMutUnpack::RawData::set_word - overwriting existing word " << word_id << ".\n";
        
      _dcm_words[word_id] = value;
      
    }
      
    //! returns address of first element in dcm word list
    const unsigned long int* get_dcm_words_address( void ) const
    { return &_dcm_words[0]; }
    
    //! return number of valid words stored (including trailer/header
    int get_n_words( void ) const
    { return _n_words; }
    
    private:
    
    //! list of data words associated to this hit
    boost::array<unsigned long int, MUIOO::kWordsPerFEM > _data_words;

    //! list of DCM words associated to this hit
    boost::array<unsigned long int, NTOTWORDS> _dcm_words;
    
    //! packet ID
    unsigned long int _packet_id;
    
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

