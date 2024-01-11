// $Id: MWGOOReco.h,v 1.1 2009/07/04 18:33:51 hpereira Exp $

#ifndef MWGOOReco_h
#define MWGOOReco_h

/*!
  \file    MWGOOReco.h
  \brief   muon nanoDST creation, using new framework input nodes
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:33:51 $
*/

#include <SubsysReco.h>
#include <PHMuoTracksOut.h>

#ifndef __CINT__
#include <TMutTrkMap.h>
#include <PHTimeServer.h>
#endif

// STL
#include <map>
#include <string>

class PHInclusiveNanoCuts;
class PHCompositeNode;

//! muon nanoDST creation, using new framework input nodes
class MWGOOReco: public SubsysReco
{
  public:
 
  //! constructor
  MWGOOReco(PHInclusiveNanoCuts *aCutter); 
  
  //! destructor
  virtual ~MWGOOReco(); 

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

};

#endif /* __MWGRECO_H__ */




