#ifndef FvtxPrimVtxEval_h
#define FvtxPrimVtxEval_h

/*!
  \file    FvtxPrimVtxEval.h
  \brief   Evaluation for FVTX primary vertex determination
  \author  Cesar da Silva
  \version $Revision: 1.3 $
  \date    $Date: 2013/04/08 22:38:43 $
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
class TFile;
class TNtuple;
class VtxOut;

//! muon nanoDST creation, using new framework input nodes
class FvtxPrimVtxEval: public SubsysReco
{
  public:
 
  //! constructor
  FvtxPrimVtxEval(std::string primaryvtxfile = ""); 
  
  //! destructor
  virtual ~FvtxPrimVtxEval(); 

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
 
  //! node where fvtxoo maps are to be found
  PHCompositeNode *_fvtxoo_node;

  int _ievt;
  TFile* _vtxfile;
  TNtuple* _vtxntup;
  VtxOut* vtxout;
  //! filename for vertex primary vertex ntuple
  std::string _primaryvtxfile;

};

#endif /* __FVTXPRIMEVAL_H__ */




