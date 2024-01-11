// $Id: mFvtxFindTrack.h,v 1.12 2011/12/01 04:16:21 slash Exp $
#ifndef __mFvtxFindTrack_HH__
#define __mFvtxFindTrack_HH__
/*!
  \file    mFvtxFindTrack.h
  \brief   Associate TFvtxCoord with TFvtxTrk using monte-carlo information (perfect pattern recognition)
  \author  Melynda Brooks
  \version $Revision: 1.12 $
  \date    $Date: 2011/12/01 04:16:21 $
*/

#include<PHTimeServer.h>
#include<vector>
#include<TFvtxTrkMap.h>
#include<TMutStraightTrackFit.h>
#include<boost/array.hpp>

#include <mFvtxModuleBase.h>

class mFvtxFindTrackPar;
class PHCompositeNode;
class TFvtxClusMap;
class TFvtxCoordMap;
class TFvtxHitMap;
class TFvtxMCHitMap;
class TFvtxPisaHitMap;
class TFvtxTrkMap;
class TMutMCTrkMap;

/*! \ingroup modules */

//! Perfect pattern recognition from monte-carlo data.

/*!  
  Initializes TFvtxTrk objects using monte-carlo data in TFvtxMCTr
  Associates TFvtxCoord with TFvtxTrk using TFvtxMCHit data.  This 
  module implements perfect pattern recognition using monte-carlo
  data. <br>

  <h2>Analysis Module Interface Specification</h2>
  <table>
  <tr>
  <td> Object </td>
  <td> Description </td>
  <td> Privilege </td>
  </tr>
  <tr>
  <td> const mFvtxFindTrackPar*</td>
  <td> Parameter Table </td>
  <td> imFvtxable </td>
  </tr>
  <tr>
  <td> TFvtxTrk*</td>
  <td> IOC</td>
  <td> Fvtxable </td>
  </tr>
  <tr>
  <td> TFvtxMCHit*</td>
  <td> IOC</td>
  <td> Fvtxable </td>
  </tr>
  <tr>
  <td> TFvtxTrk*</td>
  <td> IOC</td>
  <td> Fvtxable </td>
  </tr>
  </table>
*/


class mFvtxFindTrack : public mFvtxModuleBase
{
  public: 

  mFvtxFindTrack(); 
  virtual ~mFvtxFindTrack(){}
  virtual PHBoolean event(PHCompositeNode*);

  typedef std::pair<float,float> coord_window;

  //! coord windowing mode
  enum WindowEnum
  { IN_WINDOW, LOW_WINDOW, HIGH_WINDOW };

  void init(PHCompositeNode* top_node) {}
  void init_run(PHCompositeNode* top_node) {}
  void end(PHCompositeNode* top_node);
  
  private:  
  
  // private methods
  //! retrieve pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! load vertex
  void load_ext_vertex( PHCompositeNode* top_node );

  //! find tracks
  void find_tracks();

  //! Option to start tracks using MuTr track info
  void start_tracks_mutr();

  //! Start tracks without MuTr:  all plane 4 hits are used to start tracks
  void start_tracks_no_mutr();

  //! Set the phi and theta windows for the track, to be used in track finding
  void set_trk_windows(TFvtxTrkMap::pointer trk_ptr, PHPoint CoordMidPoint);

  //! find coordinates in a given station
  void find_coords_in_station(unsigned short station);

  //! function to clone a track and add it to the track bank if multiple candidate coordinates 
  //! are found in one station
  void clone_trk(TFvtxTrkMap::pointer in_trk_ptr, TFvtxCoordMap::pointer in_coord_ptr);

  //! function to check whether a point is within a phi window
  static WindowEnum check_phi_window(const PHPoint& point, const coord_window& phi_window); 

  //! function to check whether a point is within a theta window
  static WindowEnum check_theta_window(const PHPoint& point, const coord_window& phi_window);

  //! calculate and store initial track parameters.  Needed for KalFit to work
  void set_trk_par();

  //! check whether a candidate coordinate has an overlapping volume with a coordinate
  //! already in the track, at the same station
  bool check_detector_overlap(TFvtxCoordMap::pointer in_coord_ptr, TFvtxTrkMap::pointer trk_ptr);

  /*!
    Local Node derive from TMutStraightTrackFit abstract node, to implement constructor and fill matrices
    with the correct values from TFvtxCoord */
  class LocalNode: public TMutStraightTrackFit::Node
  {

        public:

        //! constructor (take a fvtx cluster and a reference z, phiflag indicates whether storing r or phi coord)
        LocalNode( TFvtxCoordMap::pointer coord_ptr, const double &z, const int phiflag );

  };


  //! associations between hits and tracks
  //! find coordinates in a given plane and theta/phi window
  std::list<TFvtxCoordMap::value_type>
      find_coords_in_window(TFvtxCoordMap* coord_map, unsigned short arm, int cage, int station, int sector, 
         const std::pair<float,float>& theta_window, const std::pair<float,float>& phi_window);

  void add_coords_to_trk(TFvtxTrkMap::pointer trk_ptr, std::list<TFvtxCoordMap::value_type>);

  //! associations between hits and tracks
  void promote_associations();  

  //!@name Interface pointers
  //@{

  //! module parameters
  const mFvtxFindTrackPar* _mod_par; 
  
  //! mc tracks
  TMutMCTrkMap* _mc_trk_map; 
  
  //! pisa barrel hits
  TFvtxPisaHitMap* _pisa_hit_map;
  
  //! MC hits
  TFvtxMCHitMap* _mc_hit_map; 
  
  //! tracks
  TFvtxTrkMap* _trk_map; 
  
  //! coordinates
  TFvtxCoordMap* _coord_map; 
  
  //! clusters
  TFvtxClusMap* _clus_map; 
  
  //! hits
  TFvtxHitMap* _hit_map; 
  //@}

  //! Event z vertex position
  double _vertex_z;

  //! Local coordinate list returned from window search routine
  typedef std::list<TFvtxCoordMap::value_type> local_coord_list;

  //! stores the number of tracks found in each arm.  Used to throw exeption
  //! when one arm limint on number of tracks is reached
  boost::array<size_t, FVTXOO::MAX_ARM> _n_tracks;
  
  //! module Timer
  PHTimeServer::timer _timer;

};

#endif /* __mFvtxFindTrack_HH__ */














