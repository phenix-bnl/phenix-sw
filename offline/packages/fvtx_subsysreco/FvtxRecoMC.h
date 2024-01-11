// $Id: FvtxRecoMC.h,v 1.5 2015/06/22 17:58:17 snowball Exp $
#ifndef __FvtxRecoMC_h__
#define __FvtxRecoMC_h__

/*!
  \file FvtxRecoMC.h
  \ingroup supermodules
  \brief FVTX reconstruction module, using Monte Carlo information.
        Reads a DST that has been through the FVTX response stage, creates
        TFvtxTrk objects from the MC tracks (mFvtxFindTrackMC) and then fits the
        tracks using Kalman Filter fitting (mFvtxKalFitMC).
  \author Melynda Brooks
  \version $Revision: 1.5 $
  \date $Date: 2015/06/22 17:58:17 $
*/

#include <string>
#include <MuonSubsysReco.h>

#ifndef __CINT__
#include <mFvtxFindTrackMC.h>
#include <mFvtxKalFitMC.h>
#endif

// Forward declerations
class PHCompositeNode;
class PHTimer;


//! FVTX Fast Simulation module
/*!
  \ingroup supermodules
  FVTX fast simulator. reads muid/mutr mc hit/track maps from a simulated DST
  runs the response to build muid/mutr hit maps for later reconstruction.
  \warning there is no embedding done so far in this module.
*/
class FvtxRecoMC: public MuonSubsysReco
{
  public:

  //! constructor
  FvtxRecoMC( const char* name= "FvtxRecoMC");

  //! destructor
  ~FvtxRecoMC(){ if(_timer) delete _timer;}

  //! run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event processing
  int process_event(PHCompositeNode *topNode);

  //! end of process
  int End(PHCompositeNode *topNode);

  protected:

  //! create all new nodes
  int set_node_ptrs(PHCompositeNode *topNode);

  //! create needed interfaces
  int set_interface_ptrs(PHCompositeNode *topNode);

  //! create needed modules
  int set_module_ptrs(PHCompositeNode *topNode);

  //! fvtx working node
  PHCompositeNode* _fvtx_node;

  //! dst node
  PHCompositeNode* _dst_node;

  //! gea node
  //PHCompositeNode* _gea_node;

  #ifndef __CINT__

  //! FVTX module data members
  mFvtxFindTrackMC _mFvtxFindTrackMC_mod;

  //! FVTX module data members
  mFvtxKalFitMC _mFvtxKalFitMC_mod;

  #endif
  
  //! module timer
  PHTimer* _timer;
	
};

#endif







