#ifndef __mFvtxEval_HH__
#define __mFvtxEval_HH__

#include<PHTimeServer.h>
#include<mFvtxEvalPar.h>
#include<TFvtxCoordMap.h>
#include<TFvtxTrkMap.h>
#include<TMutMCTrkMap.h>
#include<TFvtxMCHitMap.h>
#include<TFvtxEvalMap.h>

class PHCompositeNode;

//! evaluation module. Promote associations between MC and reconstructed objects
class mFvtxEval
{
  public:

  mFvtxEval();
  virtual ~mFvtxEval()
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
  void fill_trk_eval(TFvtxTrkMap::pointer, TMutMCTrkMap::pointer, TFvtxEvalMap::pointer);
  void fill_eval_res(TFvtxTrkMap::pointer, TMutMCTrkMap::pointer, TFvtxEvalMap::pointer);
  unsigned short get_true_hits(TFvtxTrkMap::pointer,TMutMCTrkMap::pointer);
  unsigned short get_n_maskhit(TMutMCTrkMap::pointer);

  // private members
  const mFvtxEvalPar* _mod_par;
  TFvtxCoordMap* _coord_map;
  TFvtxTrkMap* _trk_map;
  TFvtxEvalMap* _eval_map;
  TMutMCTrkMap* _mctrk_map;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __mFvtxEval_HH__ */













