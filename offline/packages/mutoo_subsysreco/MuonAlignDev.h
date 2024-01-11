// $Id: MuonAlignDev.h,v 1.10 2009/02/09 09:31:15 hpereira Exp $

/*!
\file    MuonAlignDev.h
\brief   mutoo reconstruction event loop
\author  Sean Kelly
\version $Revision: 1.10 $
\date    $Date: 2009/02/09 09:31:15 $
*/

#ifndef __MUONALIGNDEV_H__
#define __MUONALIGNDEV_H__

#include <string>
#include "MuonSubsysReco.h"

// Forward declerations
//

class PHCompositeNode;

#ifndef __CINT__
#include <mMutFindClus.h>
#include <mMutFitClus.h>
#include <mMutFindGapCoord.h>
#include <mMutFindStub.h>
#include <mMutStubFit.h>
#include <mMutFindTrack.h>
#include <mMutRejectTrack.h>
#include <mMutBPFit.h>
#include <mMuiFitRoadO.h>
#include <mMutMuiRoad.h>
#include <mMutStraightFit.h>
#include <mMutStraightFitPar.h>
#include <mMutFitVtx.h>

#include <PHTimeServer.h>

#endif

/*!
\class   MuonAlignDev
\brief   mutoo reconstruction event loop
*/
class MuonAlignDev: public MuonSubsysReco
{
  public:
  
  MuonAlignDev();
  
  virtual ~MuonAlignDev() 
  {}
  
  //! initialisation
  int Init(PHCompositeNode *topNode);
  
  //! run initialisation
  int InitRun(PHCompositeNode *topNode);

  //! module parameters dump
  void print_parameters() const;     
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! end of run
  int End(PHCompositeNode *topNode);
  
  //! magnets_on flag
  void set_magnets_on( const bool& value )
  { set_flag( MAGNETS_ON, value ); }
  
  //! pattern recognition flags
  enum Flag
  {
    NONE = 0,
      
    //! no anode corrections in kalman filter
    MAGNETS_ON = (1<<0),
    
    //! skip clustering
    SKIP_CLUSTERING = (1<<1),
    
    //! skip vertex fit. This is usefull notably when running on cosmics
    SKIP_VERTEX_FIT = (1<<2)
    
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
  
  protected:
  
  //! create all mutoo nodes
  int CreateNodeTree(PHCompositeNode *topNode);
  
  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned long int _flags;
  
  //! mutoo tree node
  PHCompositeNode * _mutoo_node;
  
  //! muioo tree node
  PHCompositeNode * _muioo_node;
  
  //! dst tree node
  PHCompositeNode * _dst_node;
  
  #ifndef __CINT__
  
  //!@name reconstruction modules
  //@{
  
  mMutFindClus _mMutFindClus_mod;
  
  mMutFitClus _mMutFitClus_mod;
  
  mMutFindGapCoord _mMutFindGapCoord_mod;
  
  mMutFindStub _mMutFindStub_mod;
  
  mMutStubFit _mMutStubFit_mod;
  
  mMutFindTrack _mMutFindTrack_mod;
  
  mMutRejectTrack _mMutRejectTrack_mod;
  
  mMutBPFit _mMutBPFit_mod;
  
  // reconstruction modules
  //! road fit (straight track)
  mMuiFitRoadO _mMuiFitRoadO_mod;
  
  // road to track association
  mMutMuiRoad _mMutMuiRoad_mod;
  
  //! straight track fit
  mMutStraightFit _mMutStraightFit_mod;
  
  //! vertex fit module (chisquare minimization)
  mMutFitVtx _mMutFitVtx_mod;
  
  /*! 
  straight track fit module parameters
  since the fit is done twice with different parameters (with/without muid)
  the module must be kept in object members. Should not harm.
  */
  mMutStraightFitPar* _mMutStraightFit_par;
  
  //@}
  
  //! module timer
  PHTimeServer::timer _timer;
  
  #endif
    
};

#endif /* __MUONDEV_H__ */
