// $Id: mFvtxPackPRDF.h,v 1.1 2011/04/12 18:23:32 youzy Exp $

/*!
   \file mFvtxPackPRDF.h 
   \brief module for Fvtx pack PRDF
   \author Zhengyun You
   \version $Revision: 1.1 $
   \date $Date: 2011/04/12 18:23:32 $
*/

#ifndef __mFvtxPackPRDF_HH__
#define __mFvtxPackPRDF_HH__

#include <FVTXOO_FEM.h>
#include <boost/array.hpp>
#include <TFvtxHitMap.h>
#include <PHTimeServer.h>
#include <memory>
#include <headerWrapper.h>
#include <packet_A.h>

class PHRawDataNode;
class mFvtxPackPRDFPar;
class PHCompositeNode;

/*! \ingroup modules */
//! picks hit map IOC convert to raw data and write to PRDF
/*! 
  retrieve FVTX hit map, build raw data ADC samples, writes to PRDF.
  the module must run after the response module mFvtxResponse, from MC hits
*/

class mFvtxPackPRDF
{
 public: 

  //! constructor
  mFvtxPackPRDF(); 
  
  //! destructor
  virtual ~mFvtxPackPRDF() {}; 
  
  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  

  // private methods

  //! retrieves parameter node and IOC
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! create channel data, returns true if added
  bool pack_channel_data( const TFvtxHitMap::value_type& hit);
 
  //! create string from base + row ID
  static std::string encode_string( const std::string& base, unsigned int raw_id );
 
  //! fill RawPacket DCM words corresponding to associated hits
  bool fill_dcm_words( void );
 
  //! write dcm words to PRDF node
  bool write_dcm_words( void );

  enum{ MAX_DCM_WORDS = 532, MAX_DCM_ROWS=510 };

  // Interface pointers
  TFvtxHitMap* _hit_map;			    //!< hit map IOC
  headerWrapper* _event_header; //!< event header external interface (from pisa)
  HEADER_ST* _header_table;     //!< event header table (from pisa)
  PHCompositeNode* _prdf_node;  //!< prdf node

  //! module parameter module
  mFvtxPackPRDFPar* _mod_par;
  
  // raw data array to be written to PRDF node
  boost::array<PHRawDataNode*, MAX_DCM_ROWS> _data_node_array;

  // Timer
  PHTimeServer::timer _timer;
  
  //! raw data
  class RawData
  {
    public:
    //! conststuctor
    RawData( const int& fem_id, const int& chip_id, const int& dcm_channel, const TFvtxHitMap::value_type& hit ):
      _fem_id( fem_id ),
      _chip_id( chip_id ),
      _dcm_channel( dcm_channel ),
      _hit( hit )
    {}
    
    //! retrieve fem_id
    unsigned int get_fem_id( void ) const
    { return _fem_id; }

    //! retrieve chip_id
    unsigned int get_chip_id( void ) const
    { return _chip_id; }

    //! retrieve dcm_channel
    unsigned int get_dcm_channel( void ) const
    { return _dcm_channel; }

    bool operator < (const RawData& v) const
    { return (get_fem_id() < v.get_fem_id()) ? true : false; }

    //! retrieve associated TFvtxHit
    TFvtxHitMap::value_type get_hit( void ) const
    { return _hit; }
      
    private:
    unsigned int _fem_id;         //!< fem_id
    unsigned int _chip_id;         //!< chip_id
    unsigned int _dcm_channel;    //!< dcm_channel
    TFvtxHitMap::value_type _hit; //! hit
    
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
    RawDataList& get_raw_data_list( void )
    { return _raw_data_list; }
    
    // defines DCM word
    unsigned long int set_dcm_word( int word_id, 
      unsigned long int high_bit, 
      unsigned long int data )
    { 
      // check word_id is valid
      BOUNDS_CHECK( word_id, MAX_DCM_WORDS );
      
      // check word_id against total number of valid words
      if( word_id+1 > _n_words ) _n_words = word_id + 1;     
      
      if( _dcm_words[word_id] )
      std::cerr << "mFvtxPackPRDF::RawData::set_word - overwriting existing word " << word_id << ".\n";
        
      unsigned long int word = 0x0;
      word |= (high_bit*   0x00010000);
      word |= (data&       0x0000FFFF);       
      
      _dcm_words[word_id] = word;
      return word;
    }

    // defines DCM word data
    unsigned long int set_dcm_word_data( int word_id,
      unsigned long int high_bit,
      unsigned long int dcm_channel,
      unsigned long int chip_id,
      unsigned long int adc )
    {
      // check word_id is valid
      BOUNDS_CHECK( word_id, MAX_DCM_WORDS );

      // check word_id against total number of valid words
      if( word_id+1 > _n_words ) _n_words = word_id + 1;

      if( _dcm_words[word_id] )
      std::cerr << "mFvtxPackPRDF::RawData::set_word - overwriting existing word " << word_id << ".\n";

      unsigned long int word = 0x0;
      word |= (high_bit*   0x00010000);
      word |= (dcm_channel*0x00000200);
      word |= (chip_id*    0x00000008);
      word |= (adc&        0x00000007);

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

