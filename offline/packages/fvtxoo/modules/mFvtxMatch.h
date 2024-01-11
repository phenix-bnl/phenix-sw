#ifndef __MFVTXMATCH_H__
#define __MFVTXMATCH_H__

#include<PHTimeServer.h>
//#include<mFvtxMatchPar.h>
#include<TFvtxCoordMap.h>
#include<TFvtxTrkMap.h>
#include<TMutMCTrkMap.h>
#include<TFvtxMCHitMap.h>
#include<TFvtxEvalMap.h>

class PHCompositeNode;

class mFvtxMatch
{

  public:

  mFvtxMatch();
  virtual ~mFvtxMatch()
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

  void set_dr_match(double dr) { _dR = dr; }
  void set_dm_match(double dm) { _dM = dm; }
  void set_eval_z(double z) { _eval_z = z; }

  private:

  // private methods
  void set_interface_ptrs(PHCompositeNode* signal_node, PHCompositeNode* top_node);

  void associate_mchit();
  //void associate_mctrk_nhits();
  void associate_trk_to_mctrk();

  // private members
  //const mFvtxMatchPar* _mod_par;
  TFvtxCoordMap* _coord_map;
  TFvtxTrkMap* _trk_map;
  TMutMCTrkMap* _mctrk_map;

  // Matching cuts
  //
  double _dR;
  double _dM;
  double _eval_z;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __MFVTXMATCH_HH__ */













