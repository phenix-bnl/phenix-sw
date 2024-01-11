#ifndef __MMUIEVALO_H__
#define __MMUIEVALO_H__

#include<PHTimeServer.h>
#include<mMuiEvalOPar.h>
#include<TMuiHitMapO.h>
#include<TMuiClusterMapO.h>
#include<TMuiRoadMapO.h>
#include<TMutMCTrkMap.h>
#include<TMuiMCHitMapO.h>
#include<TMuiEvalMap.h>

class mMuiEvalO
{
 public:

  mMuiEvalO();
  virtual ~mMuiEvalO(){}

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
  void fill_road_eval(TMuiRoadMapO::pointer, TMutMCTrkMap::pointer,
		      TMuiEvalMap::pointer);
  std::pair<UShort_t, UShort_t> get_true_hits(TMuiRoadMapO::pointer, TMutMCTrkMap::pointer);
  UShort_t get_n_maskhit(TMutMCTrkMap::pointer);
  void fill_eval_res(TMuiRoadMapO::pointer, TMutMCTrkMap::pointer,
		     TMuiEvalMap::pointer);

  // private members
  const mMuiEvalOPar* _mod_par;
  TMuiHitMapO* _hit_map;
  TMuiClusterMapO* _cluster_map;
  TMuiRoadMapO* _road_map;
  TMuiEvalMap* _eval_map;
  TMutMCTrkMap* _mctrk_map;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __MMUIEVALO_H__ */
