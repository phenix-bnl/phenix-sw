// $Id: mMutStraightFit.h,v 1.8 2009/04/27 11:48:43 hpereira Exp $
#ifndef __mMutStraightFit_h__
#define __mMutStraightFit_h__

/*!
  \file    mMutStraightFit.h
  \brief   straight track fit (magnetic field = 0)
  \author  Hugo Pereira
  \version $Revision: 1.8 $
  \date    $Date: 2009/04/27 11:48:43 $
*/
#include <PHGslMatrix.h>
#include <PHTimeServer.h>
#include <list>

#include <TMuiClusterMapO.h>
#include <TMuiRoadMapO.h>
#include <TMutCoordMap.h>
#include <TMutTrkMap.h>
#include <TMutStraightTrackFit.h>
#include <mMutStraightFitPar.h>

// forward declaration
class PHCompositeNode;

/*! \ingroup modules */
//! straight track fit (magnetic field = 0)
class mMutStraightFit
{

  public:
  
  //! constructor
  mMutStraightFit(); 
  
  //! destructor
  virtual ~mMutStraightFit()
  {}

  //! event method
  virtual PHBoolean event(PHCompositeNode*);	
   
  //! Set IOC pointers
  /*! it is made public to allow for single track fit from external module */
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! load vertex 
  /*! it is made public to allow for single track fit from external module */
  void load_ext_vertex( PHCompositeNode* top_node );

  //! fit track
  /*! it is made public to allow for single track fit from external module */
  bool fit_track( TMutTrkMap::pointer ) const;

  private:
     
  //! Loop over tracks
  void track_loop();
    
  /*! 
    Local Node derive from straightTrackFit abstract node, to implement constructor and fill matrices 
    with the correct values from either a TMutCoord or a TMuiCluster */
  class LocalNode: public TMutStraightTrackFit::Node
  {
  	
    public:
    
    //! constructor (take a mutr cluster and a reference z)
    LocalNode( TMutCoordMap::pointer coord_ptr, const double &z );
    
    //! constructor (take a muid cluster and a reference z)
    LocalNode( TMuiClusterMapO::pointer clus_ptr, const double &z );
  
  };

  //! fill TMutTrkObject from TrackFit object
  void fill_track( TMutTrkMap::pointer, const TMutStraightTrackFit& ) const;

  //! extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  bool extrapolate_to_vertex( TMutTrkMap::pointer trk_ptr ) const;
  
  //! adds r residual calculated from coordinate to track
  static void push_r_residual( TMutTrkMap::pointer, const TMutTrkPar&, TMutCoordMap::pointer); 
 
  //! adds w residual calculated from coordinate to track
  static void push_w_residual( TMutTrkMap::pointer, const TMutTrkPar&, TMutCoordMap::pointer); 
  
  /*! \brief
    best associated road for a given track (is based on DG0).
    Is needed to have the muid hits associated to the track.
    The corresponding TMuiClusterO are associated to the track and used in the fit.
    The other roads are dissacitiated.
  */
  void associate_road( TMutTrkMap::pointer ) const;
  
  //! vertex z
  double _vertex_z;
  
  //! pointer to the track Map
  TMutTrkMap *_trk_map;

  //! pointer to mMutStraightFit parameters module
  mMutStraightFitPar *_mod_par;	  

  //! module timer
  PHTimeServer::timer _timer;

};

#endif
