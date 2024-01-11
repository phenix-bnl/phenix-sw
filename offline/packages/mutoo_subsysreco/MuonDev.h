// $Id: MuonDev.h,v 1.29 2014/07/03 00:18:51 slash Exp $

#ifndef __MUONDEV_H__
#define __MUONDEV_H__

/*!
  \file    MuonDev.h
  \ingroup supermodules 
  \brief   mutoo reconstruction event loop, twicked to cope with Run4 Au-Au data
  \author  Sean Kelly
  \version $Revision: 1.29 $
  \date    $Date: 2014/07/03 00:18:51 $
*/

#include <string>
#include "MuonSubsysReco.h"
#include <MUTOO.h>

// Forward declerations
class PHCompositeNode;

// MUON modules
#ifndef __CINT__
#include <mMutBPFit.h>
#include <mMutBPVertex.h>
#include <mMutFindClus.h>
#include <mMutFindTrack.h>
#include <mMutFindVtx.h>
#include <mMutFitClus.h>
#include <mMutKalFit.h>
#include <mMutFitVtx.h>
#include <mMutMatchCoord.h>
#include <mMutMuiRoad.h>
#include <mMutRejectTrack.h>
#include <PHTimeServer.h>
#endif

/*!
  \class   MuonDev
  \ingroup supermodules 
  \brief   mutoo reconstruction event loop, twicked to cope with Run4 Au-Au data
*/
class MuonDev: public MuonSubsysReco
{
 public:

  //! constructor
  MuonDev( const char* name = "MUONDEV" );

  //! destructor
  ~MuonDev();
  
  //! full initialization
  int Init(PHCompositeNode *);
  
  //! full initialization
  int InitRun(PHCompositeNode *);
  
  //! event processing method
  int process_event(PHCompositeNode *);
  
  //! end of run method
  int End(PHCompositeNode *);

  //! pattern recognition flags
  enum Flag
  {
    NONE = 0,
      
    //! no anode corrections in kalman filter
    SKIP_KF_ANODES = (1<<0),
    
    /*! cluster finder, cluster fit, and gap coordinates finding
      is performed inside the mMutFindTrack modules (for octants to which
      a track from the previous station is pointing, as opposed to separate 
      modules.
    */
    USE_LOCAL_CLUSTERS = (1<<1),
    
    //! use vertex in station 123 BP fit (fixed weight)
    /*! note that the BP_VERTEX_23 is not used */
    USE_BP_VERTEX_123 = (1<<2),
    
    //! use vertex in bent plane fit
    USE_BP_VERTEX_FIXED = (1<<3),
    
    //! use momentum dependent weighted vertex in bent plane fit
    USE_BP_VERTEX_WEIGHTED = (1<<4),
    
    //! use MuID in bent plane fit
    USE_BP_MUID = (1<<5),
    
    //! hybrid cluster fit error
    USE_CLUS_HYBRID_ERROR = (1<<6),
    
    //! force three wide clusters to be fit with only one track
    FORCE_THREE_WIDE_CLUS = (1<<7),
    
    //! force use of GSL mathieson fit for all clusters of size greater than 1
    FORCE_GSL_FIT = (1<<8),
    
    //! force use of GSL mathieson fit for all clusters of size greater than 1
    FORCE_LOOKUP = (1<<9),
    
    //! disable the cluster cuts used for filtering in the cluster finding module (mMutFindClus)
    NO_CLUSTER_CUTS = (1<<10),

    //! skip clustering
    SKIP_CLUSTERING = (1<<11),

    //! skip track finding
    SKIP_TRACK_FINDING = (1<<12),
    
    //! remove ghost tracks
    /*! this flag should usually not been checked since it costs some useless overhead */
    REMOVE_GHOST_TRACKS = (1<<13),
    
    //! skip vertex fit. This is usefull notably when running on cosmics
    SKIP_VERTEX_FIT = (1<<14),
    
    //! use MUID to start tracking
    USE_MUID = (1<<15)
  };
    
  //! flags
  void set_flags( const unsigned long int& value )
  { _flags = value; }
  
  //! flags
  void set_flag( const Flag& flag, const bool& value )
  {    
    if( value ) { _flags |= flag; }
    else { _flags &= (~flag); } 
  }
    
  //! flags
  bool get_flag( const Flag& flag ) const
  { return _flags & flag; }
  
  //! kalman filter evaluation filename
  void set_kal_fit_evaluation_file( const char* file )
  { if( file ) _kal_fit_evaluation_file = file; }

  //! setter for verbosity 
  void set_verbosity(MUTOO::Verbosity v) { _verbosity = v; }

  //! getter for verbosity 
  MUTOO::Verbosity get_verbosity() const { return _verbosity; }


  protected:

  //! create all mutoo nodes
  int CreateNodeTree(PHCompositeNode *);

  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned long int _flags;
  
  //! mutoo working node
  PHCompositeNode *mutoo_node;
  
  //! dst io node
  PHCompositeNode *dst_node;
  
  #ifndef __CINT__

  //!@name reconstruction modules
  //@{
  
  //! road to track association module
  mMutMuiRoad _mMutMuiRoad_mod;
  
  //! cluster finding module
  mMutFindClus _mMutFindClus_mod;
  
  //! cluster fit module
  mMutFitClus _mMutFitClus_mod;
  
  //! coordinate matching to build gap_coordinates module
  mMutMatchCoord _mMutMatchCoord_mod;
   
  //! Track finding module
  mMutFindTrack _mMutFindTrack_mod;
  
  //! vertex finding module
  mMutFindVtx _mMutFindVtx_mod;
  
  //! track fit module (kalman filter)
  mMutKalFit _mMutKalFit_mod;
  
  //! vertex fit module (chisquare minimization)
  mMutFitVtx _mMutFitVtx_mod;
  
  //! ghost track rejection module
  mMutRejectTrack _mMutRejectTrack_mod;
    
  //! track bend plane fit module
  mMutBPFit _mMutBPFit_mod;
  
  //! vertex bend plane fit module
  mMutBPVertex _mMutBPVertex_mod;

  //@}
  
  //! Timer
  PHTimeServer::timer _timer;
  
  #endif
  
  //! kalman filter evaluation filename
  std::string _kal_fit_evaluation_file;

  //! Verbosity Flag for messaging
  MUTOO::Verbosity _verbosity;

};

#endif /* __MUONDEV_H__ */
