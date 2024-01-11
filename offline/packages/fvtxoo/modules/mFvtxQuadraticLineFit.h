#ifndef __MFVTXQUADRATICLINEFIT_H__
#define __MFVTXQUADRATICLINEFIT_H__

// Perform a straight-line fit to a set of hits, including possible VTX hits
//
#include <string>
#include <PHTimer.h>
#include <mFvtxModuleBase.h>
//#include <TMutStraightTrackFit.h>
#include <TFvtxQuadraticTrackFit.h>
#include <TFvtxCoordMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxTrkMap.h>
#include <boost/array.hpp>

#define QUAD_NPAR 6

class TTree;
class PHCompositeNode;
class TFvtxHitMap;
class TFvtxQuadraticTrkParMap;

class mFvtxQuadraticLineFit : public mFvtxModuleBase
{
public:

  mFvtxQuadraticLineFit();
  virtual ~mFvtxQuadraticLineFit();
  
  void init(PHCompositeNode* top_node);
  void init_run(PHCompositeNode* top_node) {}
  void end(PHCompositeNode* top_node);

  // Public event processor
  PHBoolean event(PHCompositeNode* topNode);

  void set_eval_filename(const char* name) { _eval_filename = name; }
  void set_do_evaluation(bool flag) { _do_evaluation = flag; }

private:
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  //! retrieve pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);
  void book_trees();
  void fill_trees(TFvtxTrkMap::const_pointer trk_ptr, 
		  const TFvtxQuadraticTrackFit& fit);

  void fit();
  void set_par(TFvtxTrkMap::pointer trk_ptr, const TFvtxQuadraticTrackFit& fit);

  //! Local nodes derived from TMutQuadraticTrackFit abstract node, to
  // implement constructor and fill matrices with the correct values
  // from measurements
  class FvtxRNode: public TFvtxQuadraticTrackFit::Node
  {
  public:
    FvtxRNode(TFvtxCoordMap::const_pointer coord_ptr);
  };

  class FvtxPhiNode: public TFvtxQuadraticTrackFit::Node
  {
  public:
    FvtxPhiNode(TFvtxCoordMap::const_pointer coord_ptr);
  };

  class VtxRNode: public TFvtxQuadraticTrackFit::Node
  {
  public:
    VtxRNode(TFvtxPisaHitMap::const_pointer coord_ptr);
  };

  class VtxPhiNode: public TFvtxQuadraticTrackFit::Node
  {
  public:
    VtxPhiNode(TFvtxPisaHitMap::const_pointer coord_ptr);
  };

  bool _do_evaluation;
  std::string _eval_filename;
  TTree* _eval_tree;
  int _ievent;
  double _vertex_z;

  // Eval variables
  int _trackid;
  int _arm;
  double _x0reco;
  double _y0reco;
  double _z0reco;
  double _mxreco;
  double _myreco;
  double _cxreco;
  double _cyreco;
  double _chi2;
  int _ndf;
  int _size;
  boost::array<double,20> _xhit;
  boost::array<double,20> _yhit;
  boost::array<double,20> _zhit;

  //! tracks
  TFvtxTrkMap* _trk_map; 
  
  //! coordinates
  TFvtxCoordMap* _coord_map; 
  
  //! hits
  TFvtxHitMap* _hit_map; 

  //! VTX hits
  TFvtxPisaHitMap* _vtxhit_map;

  //! Track fit pars
  TFvtxQuadraticTrkParMap* _par_map;

  //! module timer
  PHTimer _timer;
};
#endif /* __MFVTXQUADRATICLINEFIT_H__ */
