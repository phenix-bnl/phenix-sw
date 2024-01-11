// $Id: MWGReco.h,v 1.1 2009/07/04 18:33:51 hpereira Exp $

#ifndef MWGReco_h
#define MWGReco_h

/*!
  \file    MWGReco.h
  \brief   muon nanoDST creation, using old framework input nodes
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:33:51 $
*/

#include <SubsysReco.h>
#include <PHMuoTracksOut.h>
#include <string>

#include "PHdiMuoTracksv8.h"
#include "PHMuoTracksv8.h"

class PHInclusiveNanoCuts;
class PHCompositeNode;

//!  muon nanoDST creation, using old framework input nodes
class MWGReco: public SubsysReco
{
 public:
 
  //! constructor
  MWGReco(PHInclusiveNanoCuts *aCutter); 
  
  //! destructor
  virtual ~MWGReco(); 

  //! init method
  int Init(PHCompositeNode *top_node);
  
  //! run initialization method
  int InitRun(PHCompositeNode *top_node);
  
  //! event loop
  int process_event(PHCompositeNode *top_node);
  
  //! event reset
  int ResetEvent(PHCompositeNode *top_node);

 protected:
  
  //! old framework muon tracks loop. 
  void do_muons( PHCompositeNode *top_node, PHMuoTracksOut *particleOO );
    
  //! old framework di-muon tracks loop
  void do_dimuons(PHMuoTracksOut *particle );
  
  //! cutting module
  PHInclusiveNanoCuts *_cutter;

};

#endif /* __MWGRECO_H__ */




