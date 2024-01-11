// $Id: MutEffic.h,v 1.1 2009/07/04 18:33:52 hpereira Exp $
#ifndef MutEffic_h
#define MutEffic_h

/*!
  \file MutEffic.h
  \ingroup supermodules 
  \brief reads nanoDST and derive MuTR plane efficiency based on hit pattern
  \author H. Pereira
  \version $Revision: 1.1 $
  \date $Date: 2009/07/04 18:33:52 $
*/

#include <MUTOO.h>
#include <SubsysReco.h>
#include <TTree.h>
#include <string>
#include <PHPoint.h>
#include <PHVector.h>

#ifndef __CINT__
#include <boost/array.hpp>
#include <boost/multi_array.hpp>
#include <PHTimeServer.h>
#endif //__CINT__

// forwar declaration
class PHMuoTracksOut;
class PHCompositeNode;

//some boost types for the anode and cathode data
#ifndef __CINT__
typedef boost::multi_array<double, 3> ANODE_DATA_t;
typedef ANODE_DATA_t::index ANODE_INDEX_t;
typedef boost::multi_array<double, 4> CATHODE_DATA_t;
typedef CATHODE_DATA_t::index CATHODE_INDEX_t;
#endif //__CINT__

///! reads nanoDST and derive MuTR plane efficiency based on hit pattern
class MutEffic: public SubsysReco
{
  
  public:

  //! Verbosity enumeration
  enum Verbosity { Alot, Some, None };

  //! constructor
  MutEffic( const char* name = "MUTEFFIC", const char* file = 0 );
  
  //! destructor
  virtual ~MutEffic( void )
  {}
 
  //! changes ntuple filename
  void set_filename( const char* file )
    { if( file ) _filename = file; }
  
  //! changes ntuple filename
  void set_verbosity( const Verbosity &v )
    { _verbosity=v; }

  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! init method (begin of process)
  int Init(PHCompositeNode *topNode);
  
  //! end of process method
  int End(PHCompositeNode *topNode);

  //! get octant matching particle
  int get_octant( PHMuoTracksOut*, int ipart , int station=0);
  int get_octant( double, double );
  
  //! get half_octant matching particle
  int get_half_octant( PHMuoTracksOut*, int ipart, int station=0 );
  int get_half_octant( double, double);

  private:
  
  //! reset variables
  void reset_track_variables( void );
  
  //! returns true if track is accepted
  bool accept_track( PHMuoTracksOut*, int ) const;
  
  //! fill track in ntuple
  void fill_tree( PHMuoTracksOut*, int );

  //! intialize the geometry so we can get anodes, fems, etc.
  void init_mut_geom(int runnumber=0);

  //! figure out which anode wire this belongs to
  void get_anodes( int runnumber=0 );

  //! figure out which cathode strip this belongs to
  void get_cathodes( int runnumber=0 );

  //! figure out what the x and y are at the anode plane
  PHPoint project_to_planez( const PHPoint& start, 
			     const PHVector& momentum,
			     const double& z_end);
  
  //!@name efficiency tree
  //@{
  
  //! output filename 
  std::string _filename;
  
  //! output tree
  TTree* _tree;
  
  //! arm
  int _arm;

  //! event centrality
  int _centrality;


  #ifndef __CINT__
  
  //! position vs station
  /*! should be enough to have them stored as float */
  boost::array<float,MUTOO::MAX_STATION> _x;
  
  //! position vs station
  /*! should be enough to have them stored as float */
  boost::array<float,MUTOO::MAX_STATION> _y;

  //! position vs station
  /*! should be enough to have them stored as float */
  boost::array<float,MUTOO::MAX_STATION> _z;

  //! momentum vs station
  /*! should be enough to have them stored as float */
  boost::array<float,MUTOO::MAX_STATION> _px;
  
  //! momentum vs station
  /*! should be enough to have them stored as float */
  boost::array<float,MUTOO::MAX_STATION> _py;

  //! momentum vs station
  /*! should be enough to have them stored as float */
  boost::array<float,MUTOO::MAX_STATION> _pz;

  //! momentum at vertex
  boost::array<float, 3> _p_vtx;
 
  //! the half, gap, wire and card number for an anode
  // I would like toi use this but i guess i can not stream it out..

  //there is an anode card and wire for each gap in each station
  ANODE_DATA_t _anode;
  CATHODE_DATA_t _cathode;

  #endif

  //! track octant
  int _octant;
  
  //! track hit pattern
  int _pattern;
  
  //! track chisquare
  float _chisquare;
  
  //! road depth
  int _depth;
  
  //! DG0
  float _dg0;
  
  //! DDG0 
  float _ddg0;
  
  //! geometry init bool
  bool _geom_init;

  //! a verbosity level setting
  Verbosity _verbosity;

  //@}

  #ifndef __CINT__
  
  //! module timer
  PHTimeServer::timer _timer;
  
  #endif
  
};

#endif
