// $Id: MuonAnaTuples.h,v 1.27 2014/05/14 18:41:39 slash Exp $
#ifndef __MUONANATUPLES_H__
#define __MUONANATUPLES_H__

/*!
  \file    MuonAnaTuples.h
  \ingroup supermodules 
  \brief   basic ntuples for data evaluation and mutoo reconstruction
  \author  Hugo Pereira
  \version $Revision: 1.27 $
  \date    $Date: 2014/05/14 18:41:39 $
*/

#include "SubsysReco.h"

// Forward declerations
class PHCompositeNode;
class TNtuple;

#ifndef __CINT__
#include<bitset>
#include<TMutTrkMap.h>
#include<boost/array.hpp>
#include<PHTimeServer.h>
#endif

#include<string>
#include<PHGlobal.h>

/*!
  \class   MuonAnaTuples
  \ingroup supermodules 
  \brief   basic ntuples for data evaluation and mutoo reconstruction
*/
class MuonAnaTuples: public SubsysReco
{
 public:

  //! constructor
	MuonAnaTuples( 
    const char* name = "MUONANATUPLES", 
    const char* filename = "muon_ana_ntuples.root" 
  );

 	//! run initialization
  int Init(PHCompositeNode *topNode);
	
	//! event method
  int process_event(PHCompositeNode *topNode);
	
	//! finish run, close trees, etc.
  int End(PHCompositeNode *topNode);

  //! changes ntuple filename
  void set_filename( const char* file )
  { if( file ) _filename = file; }

  //! text file where vertex, centrality and reaction plane are saved event by event
  void set_event_filename( const char* file ) 
  { if( file ) _event_file = file; }

  //! flags
  /*! used to select which ntuples are filled/written out */
  //! evaluation flags
  enum Flag
  {
    
    //! all ntuples disabled.
    NONE = 0,
      
    //! track reconstruction
    RECO = (1<<0),
    
    //! muid hist
    MUID_HITS = (1<<1),

    //! MUIOO
    MUIOO = (1<<2),
    
    //! strip data
    STRIP = (1<<3),

    //! mutr single hits charge samples
    STPP = (1<<4),
    
    //! single hits on tracks
    TRK_STPP = (1<<5),
    
    //! mutr clusters
    CLUSTER = (1<<6),
    
    //! clusters 
    CLUSTER2 = (1<<7),    

    //! event data
    EVENT = (1<<8),
    
    //! All flags ON
    ALL_NTUPLES = (1<<9)-1,
    
    //! dump vertex to text file
    VERTEX = 1<<9,
    
    //! dump centrality to a text file
    CENTRALITY = 1<<10,
    
    //! dump reaction plane to text file
    REACTION_PLANE = 1<<11
  
  };
    
  //! flags
  void set_flags( const unsigned int& value )
  { _flags = value; }
  
  //! flags
  void set_flag( const Flag& flag, const bool& value )
  {
    if( value ) _flags |= flag;
    else _flags &= (~flag);
  }
 
  // basically for centrality, only pp would not have any centrality
  void set_species_pp( const bool& value)
  {
    if(value) _pp_flag = true;
  } 
   
  //! set the VtxOut vertex name to be used in the event file
  void set_vertex_name( std::string name ) {vertexname = name;}

  //! set the reaction plane detector id to be used in the event file
  void set_rpcode(int a) {rp_code = a;}

  //! flags
  bool get_flag( const Flag& flag ) const
  { return _flags & flag; }

  //! flags
  const unsigned int& get_flags( void ) const
  { return _flags; }  
  
  protected:

  //!@name ntuple filling methods
  //@{
  void write_reco_ntuple(PHCompositeNode*);
  void write_muid_hits(PHCompositeNode*);
  void write_muioo_ntuple(PHCompositeNode*);
  void write_stpp_ntuple(PHCompositeNode*);
  void write_trk_stpp_ntuple(PHCompositeNode*);
  void write_strip_data(PHCompositeNode*);
  void write_ClustData2_ntuple(PHCompositeNode*);
  void write_clus_ntuple(PHCompositeNode*);
  void write_event_data(PHCompositeNode*);

  //@}
  
  //! dump event information to file
  void dump_event(PHCompositeNode*);
  
  #ifndef __CINT__
  std::bitset<16> get_hit_bitset(TMutTrkMap::const_pointer trk_ptr);
  #endif
	
  private:
  
  //! flags
  unsigned int _flags;
  
  TNtuple* _muioo;
  TNtuple* _dimu_reco;
  TNtuple* _stpp;
  TNtuple* _strip_data;
  TNtuple* _event;
  TNtuple* _muid_hits;
  TNtuple* _mutr_hits;
  TNtuple* _ClustData2;
  TNtuple* _trk_stpp;
  TNtuple* _clus_reco;
  
  //! root filename
  std::string _filename;
    
  #ifndef __CINT__
  boost::array<float,14> _comp_var;
  #endif
  
  UShort_t _nvtx_o;
  UShort_t _nvtx_n;

  PHGlobal* _global;
  
  //! event file name
  /*! it is used to store event by event vertex, centrality and reaction plane information in a text file */
  std::string _event_file;             
  
  //! pointer to output event stream if any
  /*! it is used to store event by event vertex, centrality and reaction plane information in a text file */
  std::ofstream* _event_stream_ptr;     
  
  bool _pp_flag;

  std::string vertexname;

  int rp_code;

  #ifndef __CINT__
  //! module timer
  PHTimeServer::timer _timer;
  #endif

};

#endif /* __MUIDEFFIC_H__ */







