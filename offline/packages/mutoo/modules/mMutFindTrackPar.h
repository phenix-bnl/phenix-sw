// $Id: mMutFindTrackPar.h,v 1.43 2012/05/02 20:03:04 phnxbld Exp $
#ifndef __MMUTFINDTRACKPAR_HH__
#define __MMUTFINDTRACKPAR_HH__

///////////////////////////////////////////////////////////////
/*!
  \file    mMutFindTrkParKF.h
  \brief   runtime parameters for the track finding module
  \author  S. Kelly
  \version $Revision: 1.43 $
  \date    $Date: 2012/05/02 20:03:04 $
*/
///////////////////////////////////////////////////////////////

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>
#include<boost/array.hpp>

//! runtime parameters for the track finding module
class mMutFindTrackPar : public TMutParBase
{
  
  public: 
  
  /*! Default constructor */
  mMutFindTrackPar(): 
    _mode(NO_MUID),
    _init_mode(E_DPHI),
    _ref_plane(DOWNSTREAM),
    _use_local_clusters( false ),
    _min_stub_coord(2),
    _min_stub_point(0),
    _min_stub_coord_st3(2),
    _min_stub_point_st3(0),
    _dtheta(0.15),		  // radians
    _dphi(0.3),			    // radians
    _dwdz_min(0),
    _dwdz_max(6),
    _low_momentum_threshold(0.5),
    _use_stub_finder_windows(false),
    _sigma_match_dist(13),          
    _sigma_match_ang(0.18),         
    _window_mode(STUBFIT),       // STUBFIT or BPFIT
    _muid_use_golden(false),     // bool
    _max_n_tracks(50000),        // clone tracks limit in clone_trk()
    _do_evaluation( true ),      // if true, track evaluation ntuples are filled
    _evaluation_filename( "mMutFindTrack.root" ),
    _use_muid_cuts(true),
    _muid_road_depth(2),
    _muid_chipdf(200),
    _muid_nhit(5),
    _muid_max_hit_plane(10)
    {
      _muid_road_proximity_cut[MUTOO::South] = 50; 
      _muid_road_proximity_cut[MUTOO::North] = 30; 
      
      TMutParameterDB::get().get<unsigned short>( "mMutFindTrack_verbosity", _verbosity );
      TMutParameterDB::get().get<Double_t>( "mMutFindTrack_low_momentum_threshold", _low_momentum_threshold );
      TMutParameterDB::get().get<unsigned short>( "mMutFindTrack_min_stub_coord", _min_stub_coord );
      TMutParameterDB::get().get<Double_t>( "mMutFindTrack_dtheta", _dtheta );
      TMutParameterDB::get().get<Double_t>( "mMutFindTrack_dphi", _dphi );
      TMutParameterDB::get().get<bool>( "mMutFindTrack_do_evaluation", _do_evaluation );
      TMutParameterDB::get().get<std::string>( "mMutFindTrack_evaluation_filename", _evaluation_filename );
      TMutParameterDB::get().get<bool>( "mMutFindTrack_use_muid_cuts", _use_muid_cuts );
      
    }
  
  /*! Destructor */
  ~mMutFindTrackPar(){;}
  
  /*! Mode */
  enum Mode { USE_MUID, NO_MUID};
  
  /*! Mode */
  Mode get_mode() const { return _mode;}
  
  /*! Mode */
  void set_mode(Mode mode) { _mode = mode;}
    
  /*! Stub-finding Window Mode */
  enum WindowMode { STUBFIT, BPFIT};
  
  /*! stub-finding window mode */
  WindowMode get_window_mode() const 
  { return _window_mode;}

  /*! Stub-finding Window Mode */
  void set_window_mode(WindowMode window_mode) 
  { _window_mode = window_mode;}

  /*! return clone tracks limit for mMutFindTrack::clone_trk()*/
  unsigned short get_max_n_tracks() const 
  { return _max_n_tracks;}

  /*! set clone tracks limit for mMutFindTrack::clone_trk()*/
  void set_max_n_tracks(unsigned short max) 
  { _max_n_tracks = max;}

  /*! Init Mode */
  enum InitMode 
  { FIXED, DPHI, E_DPHI };

  /*! Init Mode */
  InitMode get_init_mode() const 
  { return _init_mode;}
  
  /*! Init Mode */
  void set_init_mode(InitMode init_mode) 
  { _init_mode = init_mode;}
  
  /*! Init Mode */
  enum RefPlane 
  { UPSTREAM, DOWNSTREAM };
  
  /*! Reference Plane */
  const RefPlane& get_ref_plane() const 
  { return _ref_plane;}
  
  /*! Reference Plane */
  void set_ref_plane( const RefPlane& ref_plane) 
  { _ref_plane = ref_plane;}
  
  /*! local cluster finder and fit */
  const bool& get_use_local_clusters( void ) const
  { return _use_local_clusters; }
  
  /*! local cluster finder and fit */
  void set_use_local_clusters( const bool& value ) 
  { _use_local_clusters = value; }
  
  /*! Minimum coordinates for valid stub station 1 or station 2*/
  const unsigned short& get_min_stub_coord() const 
  { return _min_stub_coord; }
  
  /*! Minimum coordinates for valid stub station 1 or station 2*/
  void set_min_stub_coord( const unsigned short& min_stub_coord)  
  { _min_stub_coord = min_stub_coord; }
  
  /*! Minimum coordinates for valid stub station 3 */
  const unsigned short& get_min_stub_coord_st3() const 
  { return _min_stub_coord_st3; }
  
  /*! Minimum coordinates for valid stub station 3 */
  void set_min_stub_coord_st3( const unsigned short& min_stub_coord_st3)  
  { _min_stub_coord_st3 = min_stub_coord_st3; }
  
  /*! Minimum points for valid stub station 1 or station 2 */
  const unsigned short& get_min_stub_point() const 
  { return _min_stub_point; }
  
  /*! Minimum points for valid stub station 1 or station 2 */
  void set_min_stub_point( const unsigned short& min_stub_point)  
  { _min_stub_point = min_stub_point; }
  
  /*! Minimum points for valid stub station 1 or station 2 */
  const unsigned short& get_min_stub_point_st3() const 
  { return _min_stub_point_st3; }
  
  /*! Minimum points for valid stub station 1 or station 2 */
  void set_min_stub_point_st3( const unsigned short& min_stub_point_st3)  
  { _min_stub_point_st3 = min_stub_point_st3; }
  
  /*! Delta theta for window */
  const double& get_dtheta() const 
  { return _dtheta; }
  
  /*! Delta theta for window */
  void set_dtheta( const double& dtheta) 
  { _dtheta = dtheta; }
  
  /*! Delta phi for window */
  const double& get_dphi() const 
  { return _dphi; }
  
  /*! Delta phi for window */
  void set_dphi( const double& dphi) 
  { _dphi = dphi; }
  
  /*! Stub dwdz */
  const double& get_dwdz_min() const 
  { return _dwdz_min; }
  
  /*! Stub dwdz */
  void set_dwdz_min( const double& dwdz_min) 
  { _dwdz_min = dwdz_min; }
  
  /*! Stub dwdz */
  const double& get_dwdz_max() const 
  { return _dwdz_max; }
  
  /*! Stub dwdz */
  void set_dwdz_max( const double& dwdz_max) 
  { _dwdz_max = dwdz_max; }
  
  /*! Flag tracks with estimated momentum below given threshold */
  const double& get_low_momentum_threshold() const 
  { return _low_momentum_threshold; }
  
  /*! Flag tracks with estimated momentum below given threshold */
  void set_low_momentum_threshold( const double& low_momentum_threshold) 
  { _low_momentum_threshold = low_momentum_threshold; }
  
  /*! Apply windows at the stub finder stage */
  const bool& get_use_stub_finder_windows() const 
  { return _use_stub_finder_windows; }
  
  /*! Apply windows at the stub finder stage */
  void set_use_stub_finder_windows( const bool& use_stub_finder_windows) 
  { _use_stub_finder_windows = use_stub_finder_windows; }

  //!@name matching between Muid and track/stubs
  //@{
  
  /*! Muid Road proximity cut used during select stub */
  const double& get_muid_road_proximity_cut( const unsigned short& arm) const 
  { return _muid_road_proximity_cut[arm];}
  
  /*! Muid Road proximity cut used during select stub */
  void set_muid_road_proximity_cut( const unsigned short& arm, const double& muid_road_proximity_cut) 
  { _muid_road_proximity_cut[arm]=muid_road_proximity_cut; }  
  
  /*! Sigma of the distribusion of distance between the muid road projection point at last cathode plane of station3 and
  the station3 stub projection point at last cathode plane of station3 */
  void set_sigma_match_dist( const double& sigma_match_dist) 
  { _sigma_match_dist = sigma_match_dist;}
  
  /*! Sigma of the distribusion of distance between the muid road projection point at last cathode plane of station3 and
  the station3 stub projection point at last cathode plane of station3 */
  const double& get_sigma_match_dist() const 
  { return _sigma_match_dist;}
  
  /*! Sigma of the distribusion of angle between the muid road direction vector and
  the station3 stub direction vector. */
  void set_sigma_match_ang( const double& sigma_match_ang) 
  { _sigma_match_ang = sigma_match_ang;}
  
  /*! Sigma of the distribusion of angle between the muid road direction vector and
  the station3 stub direction vector. */
  const double& get_sigma_match_ang() const 
  { return _sigma_match_ang;}
  
  //@}
  
  //!@name Muid road cuts
  //@{

  /*! Use only golden MuID roads or not*/
  bool get_muid_use_golden() const 
  { return _muid_use_golden;}

  /*! Use only golden MuID roads or not*/
  void set_muid_use_golden(bool muid_use_golden) 
  { _muid_use_golden = muid_use_golden;}
  
  /*! Use MUID cuts */
  const bool& get_use_muid_cuts() const 
  { return _use_muid_cuts; }
  
  /*! Use MUID cuts */
  void set_use_muid_cuts( const bool& use_muid_cuts) 
  { _use_muid_cuts = use_muid_cuts; }
  
  /*! Muid Road depth cut */
  const unsigned short& get_muid_road_depth() const 
  { return _muid_road_depth; }
  
  /*! MuID Road depth cut  */
  void set_muid_road_depth( const unsigned short& muid_road_depth) 
  { _muid_road_depth = muid_road_depth; }
  
  /*! muid road chisquare per degree of freedom */
  const double& get_muid_chipdf() const 
  { return _muid_chipdf; }
  
  /*! muid road chisquare per degree of freedom */
  void set_muid_chipdf( const double& muid_chipdf) 
  { _muid_chipdf = muid_chipdf; }
  
  /*! number of hits in muid road */
  const unsigned short& get_muid_nhit() const 
  { return _muid_nhit; }
  
  /*! number of hits in muid road */
  void set_muid_nhit( const unsigned short& muid_nhit) 
  { _muid_nhit = muid_nhit; }
  
  /*!  */
  const unsigned short& get_muid_max_hit_plane() const 
  { return _muid_max_hit_plane; }
  
  /*!  */
  void set_muid_max_hit_plane( const unsigned short& muid_max_hit_plane) 
  { _muid_max_hit_plane = muid_max_hit_plane; }
  
  //@}
  
  //!@name evaluation flags
  //@{
  
  /*! track evaluation flag */
  void set_do_evaluation( const bool& value )
  { _do_evaluation = value; }
  
  /*! track evaluation flag */
  const bool& get_do_evaluation( void ) const
  { return _do_evaluation; }
  
  /*! track evaluation filename */
  void set_evaluation_filename( const std::string& value )
  { _evaluation_filename = value; }
  
  /*! track evaluation filename */
  const std::string& get_evaluation_filename( void ) const
  { return _evaluation_filename; }
  
  //@}
  
  //! print parameters
  void print( std::ostream& out = std::cout ) 
  {
    MUTOO::PRINT( out, "mMutFindTrackPar" );
    out << "_mode = " << _mode << std::endl;
    out << "_init_mode = " << _init_mode << std::endl;
    out << "_ref_plane = " << _ref_plane << std::endl;
    out << "_use_local_clusters = " << _use_local_clusters << std::endl;
    out << "_min_stub_coord = " << _min_stub_coord << std::endl;
    out << "_min_stub_point = " << _min_stub_point << std::endl;
    out << "_min_stub_coord_st3 = " << _min_stub_coord_st3 << std::endl;
    out << "_min_stub_point_st3 = " << _min_stub_point_st3 << std::endl;
    out << "_dtheta = " << _dtheta << " rad" << std::endl;
    out << "_dphi = " << _dphi << " rad" << std::endl;
    out << "_dwdz_min = " << _dwdz_min << std::endl;
    out << "_dwdz_max = " << _dwdz_max << std::endl;
    out << "_low_momentum_threshold = " << _low_momentum_threshold << std::endl;
    out << "_use_stub_finder_windows = " << _use_stub_finder_windows << std::endl;
    out << "_window_mode = " << _window_mode << std::endl;
    out << "_max_n_tracks = " << _max_n_tracks << std::endl;
    out << std::endl;

    out << "Matching between muid and stubs/tracks" << std::endl; 
    out << "_sigma_match_dist = " << _sigma_match_dist << std::endl;          
    out << "_sigma_match_ang = " << _sigma_match_ang << std::endl;         
    out << "_muid_road_proximity_cut south = " << _muid_road_proximity_cut[MUTOO::South] << " cm.\n";
    out << "_muid_road_proximity_cut north = " << _muid_road_proximity_cut[MUTOO::North] << " cm.\n";
    out << std::endl;

    out << "Evaluation flags" << std::endl; 
    out << "_do_evaluation = " << _do_evaluation << std::endl;
    out << "_evaluation_filename = " << _evaluation_filename << std::endl;
    out << std::endl;
    
    out << "Muid road selection cuts" << std::endl; 
    out << "_muid_use_golden = " << _muid_use_golden  << std::endl;
    out << "_use_muid_cuts = " << _use_muid_cuts << std::endl;
    out << "_muid_road_depth = " << _muid_road_depth << std::endl;
    out << "_muid_chipdf = " << _muid_chipdf << std::endl;
    out << "_muid_nhit = " << _muid_nhit << std::endl;
    out << "_muid_max_hit_plane = " << _muid_max_hit_plane << std::endl;
    MUTOO::PRINT( out, "**" );
  }
  
  private:  
  
  /*! \brief 
  MuidRoad mode. If USE_MUID only tracks for octants pointed to by muid
  roads are found
  */
  Mode _mode;
  
  InitMode _init_mode;
  RefPlane _ref_plane;

  //! true when local cluster finding and fit is to be used
  bool _use_local_clusters;

  unsigned short _min_stub_coord;
  unsigned short _min_stub_point;
  unsigned short _min_stub_coord_st3;
  unsigned short _min_stub_point_st3;
  double _w_prox_cut;
  double _dca_cut;
  double _dtheta;
  double _dphi;
  double _coord_proximity_cut;
  
  //! road to coordinate proximity cut
  boost::array<double,MUTOO::NumberOfArms> _muid_road_proximity_cut;
  double _v0_r_cut;
  double _dwdz_min;
  double _dwdz_max;
  double _low_momentum_threshold;
  bool _use_stub_finder_windows;
  double _sigma_match_dist;
  double _sigma_match_ang;
  WindowMode _window_mode;
  bool _muid_use_golden;
  unsigned short _max_n_tracks;
  
  //! if true track evaluation tree is created, filled.
  bool _do_evaluation;   
  
  //! evaluation filename
  std::string _evaluation_filename;
  
  //! if true, add cuts on muid road before finding roads
  bool _use_muid_cuts;
  
  //! muid road depth cut
  unsigned short  _muid_road_depth;
  
  //! muid chisquare/ndf cut
  double  _muid_chipdf;
  
  //! muid min number of hits cut
  unsigned short  _muid_nhit;
  
  //! muid max hit plane cut
  unsigned short  _muid_max_hit_plane;
  
};

#endif /* __MMUTFINDTRACKPAR_HH__ */












