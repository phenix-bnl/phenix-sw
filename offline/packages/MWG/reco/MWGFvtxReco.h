// $Id: MWGFvtxReco.h,v 1.10 2015/06/04 21:46:22 snowball Exp $

#ifndef MWGFvtxReco_h
#define MWGFvtxReco_h

/*!
  \file    MWGFvtxReco.h
  \brief   muon nanoDST creation including FVTX
  \author  Cesar da Silva
  \version $Revision: 1.10 $
  \date    $Date: 2015/06/04 21:46:22 $
*/

#include <SubsysReco.h>
#include <PHMuoTracksOut.h>
#include "mMutKalFitWithSiliRealPar.h"

#ifndef __CINT__
#include <TMutTrkMap.h>
#include <PHTimeServer.h>
#endif

// STL
#include <map>
#include <string>

class PHInclusiveNanoCuts;
class PHCompositeNode;
class PHTrackIntegratorKF;

//! muon nanoDST creation, using new framework input nodes
class MWGFvtxReco: public SubsysReco
{
  public:
 
  //! constructor
  MWGFvtxReco(PHInclusiveNanoCuts *aCutter); 
  
  //! destructor
  virtual ~MWGFvtxReco(); 

  //! init method
  int Init(PHCompositeNode *top_node);
  
  //! run initialization method
  int InitRun(PHCompositeNode *top_node);
  
  //! event loop
  int process_event(PHCompositeNode *top_node);
  
  //! End
  int End(PHCompositeNode *top_node);

 protected:

  //! get pointers to needed nodes
  bool set_node_ptrs(PHCompositeNode* top_node);
  
  /*! 
    \brief new framework muon tracks loop. 
    returns mapping between new framework track unique id and local track id
  */
  std::map< ULong_t, int > do_muons( PHMuoTracksOut *particle );
  
  //! new framework di-muon tracks loop
  void do_dimuons( PHMuoTracksOut *particle, const std::map<ULong_t,int>& keys );
  
  #ifndef __CINT__
  //! check if tracks are to be accepted
  bool accept_track( TMutTrkMap::const_pointer ) const;
  
  //! Timer
  PHTimeServer::timer _timer;
  #endif

  //! cutting module
  PHInclusiveNanoCuts *_cutter;
  
  //! ndst node
  PHCompositeNode *_ndst_node;
  
  //! node where mutoo maps are to be found
  PHCompositeNode *_mutoo_node;     
  
  //! node where muioo maps are to be found
  PHCompositeNode *_muioo_node;     

  //! node where fvtxoo maps are to be found
  PHCompositeNode *_fvtxoo_node;

  //! pointer to mMutKalFitWithSiliReal parameters module
  mMutKalFitWithSiliRealPar* _mod_par;		

  //! pointer to PHTrackIntegratorKF
  PHTrackIntegratorKF *integrator;

  int _ievt;
};

#endif /* __MWGRECO_H__ */




