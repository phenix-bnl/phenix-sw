#ifndef mFvtxSlowSim_h
#define mFvtxSlowSim_h

#include <FVTXOO.h>
#include <mFvtxSlowSimPar.h>
#include <PHTimeServer.h>
#include <SvxPisaHit.h>
#include <TFvtxIndex.h>
#include <TFvtxMCHitMap.h>
#include <TTree.h>
#include <TFvtxPisaHitMap.h>

class TFvtxPisaHitMap;
class TMCPrimaryMap;
class TMutMCTrkMap;

class mFvtxSlowSim
{
  public:

  //! constructor
  mFvtxSlowSim();

  //! destructor
  ~mFvtxSlowSim()
  {}

  //! event method
  PHBoolean event(PHCompositeNode*);

  //! print summary of acceptance rejected hits
  void print_summary( std::ostream& out = std::cout );

  //! get local copy of needed nodes
  void set_interface_ptrs(PHCompositeNode*);

  private:

  //! loop over pisa hits, converts into TFvtxMCHit
  void simulator_loop(void);

  //! associate MC tracks and hits
  void associate_mc_trk(TFvtxMCHitMap::pointer);

  //! associate a FvtxMCHit with its child MC tracks
  void associate_child_mc_trk(TFvtxMCHitMap::pointer);

  //! associate MC tracks and pisa hits
  void associate_mc_trk(TFvtxPisaHitMap::pointer, int track_id);

  //! create a new MC track
  void fill_new_mc_trk(TFvtxMCHitMap::pointer, int trackID = 0);

  //! retrieve hit detector index from pisa hit position
  TFvtxIndex get_index_from_position( const int& pisa_hit_id, bool& valid ) const;

  //! retrieve hit detector index from pisa hit volume IDs
  TFvtxIndex get_index_from_volume_id( const int& pisa_hit_id, bool& valid ) const;

  //! consistency check
  /*!
    some basic checks are performed between the pisa hit position,
    the detector index retrieved from pisa volume ids
    and the detector index retrieved from pisa hit position
  */
  bool check_consistency( const int& pisa_hit_id, TFvtxIndex volume_id, TFvtxIndex position_id );

  //! book evaluation tree
  void book_evaluation_tree( void );

  //! fill evaluation tree
  void fill_evaluation_tree( const int& pisa_hit_id, const TFvtxIndex& index, const bool& valid );

  //! Module parameter table
  const mFvtxSlowSimPar* _mod_par;

  //! pisa hits container
  TFvtxPisaHitMap* _pisa_hit_map;
  
  //! mc hits container
  TFvtxMCHitMap* _mc_hit_map;

  //! mc trks container
  TMutMCTrkMap* _mc_trk_map;

  //svx hit object
  SvxPisaHit* _svx;

  //! module timer
  PHTimeServer::timer _timer;

  //! number of total PISA hits/arm
  boost::array< unsigned int, FVTXOO::MAX_ARM > _total_pisa_hits_arm;

  //! number of accepted PISA hits/arm
  boost::array< unsigned int, FVTXOO::MAX_ARM > _accepted_pisa_hits_arm;

  //! number of total PISA hits/arm/cage
  boost::array< unsigned int, mFvtxSlowSimPar::n_acceptance_parameters > _total_pisa_hits_cage;

  //! number of accepted PISA hits/arm/cage
  boost::array< unsigned int, mFvtxSlowSimPar::n_acceptance_parameters > _accepted_pisa_hits_cage;

  //! number of total PISA hits/arm/cage/station
  boost::array< unsigned int, mFvtxSlowSimPar::n_acceptance_parameters > _total_pisa_hits_station;

  //! number of accepted PISA hits/arm/station
  boost::array< unsigned int, mFvtxSlowSimPar::n_acceptance_parameters > _accepted_pisa_hits_station;

  //! error counts
  unsigned int _errors_z;
  
  //! error counts
  unsigned int _errors_id;
  
  //! wrapper for pisa hit
  class PisaHitWrapper
  {

    public:

    //! constructor
    PisaHitWrapper( SvxPisaHit* svx, const int& hit_id ):
      _x( svx->GetHit(hit_id)->GetXGlobal() ),
      _y( svx->GetHit(hit_id)->GetYGlobal() ),
      _z( svx->GetHit(hit_id)->GetZGlobal() ),
      _track( svx->GetHit(hit_id)->GetTrack() ),
      _mc_track( svx->GetHit(hit_id)->GetMctrack() ),
      _layer( svx->GetHit(hit_id)->GetLayer() )
    {
      for( int i=0; i<9; i++ )
      _volume_id[i] = svx->GetHit(hit_id)->GetHitVolume(i);
    }

    //! position
    double _x;

    //! position
    double _y;

    //! position
    double _z;

    //! track id
    int _track;

    //! mc track id
    int _mc_track;

    //! layer id
    int _layer;

    // PISA volume ids
    boost::array<int, 9> _volume_id;

    //! streamer
    friend std::ostream& operator << (std::ostream& out, const PisaHitWrapper& hit )
    {
      out
        << " position=(" << hit._x << "," << hit._y << "," << hit._z << ")"
        << " layer=" << hit._layer
        << " volume= [ ";
      for( int i=0; i<9; i++ ) out << hit._volume_id[i] << " ";
      out << "]";
      return out;
    }

  };

  //!@name evaluation tree
  //@{

  //! pisa evaluation tree
  TTree* _pisa_tree;

  //! event counter
  int _evt;

  //!@name location
  //@{

  //! offline arm location
  int _arm;

  //! offline cage
  int _cage;

  //! offline station
  int _station;

  //! offline sector
  int _sector;

  //! offline column
  int _column;

  //@}

  //!@name global (center) position
  //@{
  //! position
  double _pisa_x;

  //! position
  double _pisa_y;

  //! position
  double _pisa_z;

  //! position
  double _pisa_r;

  //! position
  double _pisa_phi;

  //@}

  //!@name global in/out position (directly from pisa tree)
  //@{

  //! position
  double _pisa_x_in;

  //! position
  double _pisa_y_in;

  //! position
  double _pisa_z_in;

  //! position
  double _pisa_x_out;

  //! position
  double _pisa_y_out;

  //! position
  double _pisa_z_out;
  //@}

  //!@name local in/out position (directly from pisa)
  //@{

  //! position
  double _pisa_x_local_in;

  //! position
  double _pisa_y_local_in;

  //! position
  double _pisa_z_local_in;

  //! position
  double _pisa_x_local_out;

  //! position
  double _pisa_y_local_out;

  //! position
  double _pisa_z_local_out;

  //@}

  //! PISA hit energy loss
  double _eloss;

  //! PISA hit path length
  double _path_length;

  //! true if a valid MC hit was created
  int _valid;

  /*!
    offline Column z position, for
    consistency check
  */
  double _radius_z;

  //! column radius
  double _radius_inner_radius;

  //! column radius
  double _radius_outer_radius;

  //! column phi angle
  double _radius_phi_begin;

  //! column phi angle
  double _radius_phi_end;

  //! column phi angle
  double _radius_phi;

  //! volume id
  boost::array<int, 9> _volume_id;

  // pisa layer ID
  int _layer;

  //@}
  
  //!@name geometry check
  
  //@{
  //! volume geometry description
  class VolumeGeometry
  {
    
    public:
    
    VolumeGeometry( void ):
      _z(0),
      _r_min(0),
      _r_max(0),
      _phi_min(0),
      _phi_max(0)
      {}
    
    double _z;
    double _r_min;
    double _r_max;
    double _phi_min;
    double _phi_max;
    
    //! streamer
    friend std::ostream& operator << (std::ostream& out, const VolumeGeometry& geom )
    {
      out << "z: " << geom._z 
        << " r: (" << geom._r_min << "," << geom._r_max << ")"
        << " phi: (" << geom._phi_min << "," << geom._phi_max << ")";
      return out;
    }
    
  };  

  //! map volume geometry to PISA hit detector index
  typedef std::map< TFvtxIndex, VolumeGeometry > GeometryMap;
  GeometryMap _geometry;
  
  //@}
  
};

#endif /* __MFVTXSLOWSIM_HH__ */
