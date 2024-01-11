#ifndef __MMUTEVAL_HH__
#define __MMUTEVAL_HH__

#include<PHTimeServer.h>
#include<mMutEvalPar.h>
#include<TMutCoordMap.h>
#include<TMutTrkMap.h>
#include<TMutMCTrkMap.h>
#include<TMutMCHitMap.h>
#include<TMutEvalMap.h>

class PHCompositeNode;

//! evaluation module. Promote associations between MC and reconstructed objects
class mMutEval
{
 public:

  mMutEval();
  virtual ~mMutEval()
  {}

  //! event method
  virtual PHBoolean event(PHCompositeNode* top_node)
  { return event( top_node, top_node ); }

  //! event method
  /*!
  two nodes are passed to handle cases where signal and working nodes are not under the
  same top node in the tree, notably for embedding
  */
  virtual PHBoolean event(PHCompositeNode* signal_node, PHCompositeNode* top_node);

 private:

  // private methods
  void set_interface_ptrs(PHCompositeNode* signal_node, PHCompositeNode* top_node);
  void evaluate();
  void associate_mchit();
  void associate_mctrk();
  void fill_trk_eval(TMutTrkMap::pointer, TMutMCTrkMap::pointer, TMutEvalMap::pointer);
  void fill_eval_res(TMutTrkMap::pointer, TMutMCTrkMap::pointer, TMutEvalMap::pointer);
  unsigned short get_true_hits(TMutTrkMap::pointer,TMutMCTrkMap::pointer);
  unsigned short get_n_maskhit(TMutMCTrkMap::pointer);

  // private members
  const mMutEvalPar* _mod_par;
  TMutCoordMap* _coord_map;
  TMutTrkMap* _trk_map;
  TMutEvalMap* _eval_map;
  TMutMCTrkMap* _mctrk_map;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __MMUTEVAL_HH__ */













