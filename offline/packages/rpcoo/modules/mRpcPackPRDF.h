#ifndef __mRpcPackPRDF_HH__
#define __mRpcPackPRDF_HH__

#include <boost/array.hpp>
#include <boost/shared_ptr.hpp>
#include <TRpcHitMap.h>
#include <PHTimeServer.h>
#include <RPCOO.h>
#include <headerWrapper.h>

class RpcTriggerMap;
class MutDCMChannelMap;
class PHRawDataNode;

class mRpcPackPRDF
{
 public: 
  
  mRpcPackPRDF(); 
  
  virtual ~mRpcPackPRDF() {} 
  
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  
  
  // private methods
  
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  bool pack_channel_data( const TRpcHitMap::value_type& hit);

  bool pack_rpc1_data( const TRpcHitMap::value_type& hit);
  
  int setbits(int mod, int ch, int time1, int time2);
  
  bool fill_dcm_words( void );
  
  bool write_dcm_words( void );

  //For checking purposes: set up for run 10
  int checkStrip(int packet, int data);
  int getRPCCh( int fPacket, int fGlobalChannel,
		int &fOctant, int &fHalfOct, int &fRadSeg, int &fStrip);
  int checkStripRPC1(int packet, int data);
  int getRPC1Ch( int fPacket, int fGlobalChannel,
		int &fOctant, int &fHalfOct, int &fRadSeg, int &fStrip);

  // Interface pointers
  TRpcHitMap* _hit_map; 
  headerWrapper* _event_header; 
  HEADER_ST* _header_table;     
  PHCompositeNode* _prdf_node;  

  RpcTriggerMap *fRpcTrigMap;
  
  // raw data array to be written to PRDF node
  static const int size = 14;//run 11->10 run 12->14
  boost::array< PHRawDataNode*, size> _data_node_array;
  
  // Timer
  PHTimeServer::timer _timer;
  
  //Storage class for the hit/bit
  class RpcData
  {
  public:
    RpcData( 
	    const int id_soft,
	    const int id_hard,
	    const TRpcHitMap::value_type& hit 
	    ):
      _id_soft( id_soft ),
      _id_hard( id_hard ),
      _hit( hit )
	{}
      
      int get_id_soft( void ) const
      { return _id_soft; }
      
      int get_id_hard( void ) const
      { return _id_hard; }
      
      TRpcHitMap::value_type get_hit( void ) const
	{ return _hit; }
      
  private:
      int _id_soft; 
      int _id_hard; 
      TRpcHitMap::value_type _hit;
      
      
  };
  
  // shortcut for RpcData list
  typedef std::list<RpcData> RpcDataList;
  
  //Packet container class for the RPC
  class RpcPacket
  {
    
  public:
    
    RpcPacket( void ):
      _packet_id( 0 ),
      _n_words( 0 )
	{ 
	  _dcm_words.assign( 0 );  
	}
      
      //Setters:
      void set_packet_id( unsigned long int id ) { _packet_id = id; }            
      void add_rpc_data( const RpcData& data ) { _rpc_data_list.push_back( data ); }
      void set_dcm_word(int word_id,  unsigned long int value) {
	if( word_id+1 > _n_words ) { _n_words = word_id + 1; }
	if( _dcm_words[word_id] ) { 
	  std::cerr << "mRpcUnpackPRDF::RpcData::set_word - overwriting existing word "
		    << word_id << ".\n"; }
	_dcm_words[word_id] = value; }


      //Getters:
      unsigned long int get_packet_id( void )      const   { return _packet_id; }
      const RpcDataList& get_rpc_data_list( void ) const   { return _rpc_data_list; }
      const unsigned long int* get_dcm_words_address( void ) const { return &_dcm_words[0]; }
      int get_n_words( void ) const { return _n_words; }
      
  private:
            
      boost::array<unsigned long int, 250 > _dcm_words;//250 is the maximum number in the array, this needs to be corrected
      unsigned long int _packet_id;
      
      int _n_words;
      
      RpcDataList _rpc_data_list; 
      
  };
  
  typedef std::map< unsigned int, RpcPacket > RpcPacketMap;//Packet Number, RpcPacket
  RpcPacketMap _rpc_packet_map;
  
};

#endif /* __mRpcPackPRDF_HH__ */
 
