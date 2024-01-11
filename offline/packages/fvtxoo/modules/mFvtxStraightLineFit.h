
// Perform a straight-line fit to a set of hits, including possible VTX hits
//
#include <string>
#include <PHTimer.h>
#include <mFvtxModuleBase.h>
#include <TMutStraightTrackFit.h>
#include <TFvtxCoordMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxSvxClusterMap.h>
#include <TFvtxTrkMap.h>
#include <PHTimeServer.h>
#include <boost/array.hpp>

class TTree;
class PHCompositeNode;
class TFvtxHitMap;
class TFvtxResidualMap;
class TFvtxStraightTrkParMap;

class mFvtxStraightLineFit : public mFvtxModuleBase
{
public:

  mFvtxStraightLineFit();
  virtual ~mFvtxStraightLineFit();
  
  void init(PHCompositeNode* top_node);
  void init_run(PHCompositeNode* top_node) {}
  void end(PHCompositeNode* top_node);

  // Public event processor
  PHBoolean event(PHCompositeNode* topNode);

  void set_eval_filename(const char* name) { _eval_filename = name; }
  void set_do_evaluation(bool flag) { _do_evaluation = flag; }
  //! set using svx pisa hit or cluster
  void set_use_svx_cluster(bool flag) { _use_svx_cluster = flag;}

private:
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  //! retrieve pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node);
  void book_trees();
  void fill_trees(TFvtxTrkMap::const_pointer trk_ptr, 
		  const TMutStraightTrackFit& fit);

  void fit();
  void set_par(TFvtxTrkMap::pointer trk_ptr, const TMutStraightTrackFit& fit);

  //! Local nodes derived from TMutStraightTrackFit abstract node, to
  // implement constructor and fill matrices with the correct values
  // from measurements
  class Node : public TMutStraightTrackFit::Node {};
//   class Node : public TMutStraightTrackFit::Node
//   {
//     double _r;
//     double _phi;
//     double _z;

//     // Set values in the local coords of the measurement
//     void set_point(double r, double phi, double z)
//     {
//       _r = r;
//       _phi = phi;
//       _z = z;
//     }
//     double get_r() const { return _r; }
//     double get_phi() const { return _phi; }
//     double get_z () const { return _z; }
//   }
  class FvtxRNode: public Node
  {
  public:
    FvtxRNode(TFvtxCoordMap::const_pointer coord_ptr);
  };

  class FvtxPhiNode: public Node
  {
  public:
    FvtxPhiNode(TFvtxCoordMap::const_pointer coord_ptr);
  };

  class VtxRNode: public Node
  {
  public:
    VtxRNode(TFvtxPisaHitMap::const_pointer coord_ptr, 
	     const double zsmear, const double phismear);
    VtxRNode(TFvtxSvxClusterMap::const_pointer coord_ptr, 
	     const double zsmear, const double phismear);
  };

  class VtxPhiNode: public Node
  {
  public:
    VtxPhiNode(TFvtxPisaHitMap::const_pointer coord_ptr, 
	       const double zsmear, const double phismear);
    VtxPhiNode(TFvtxSvxClusterMap::const_pointer coord_ptr, 
	       const double zsmear, const double phismear);
  };

  typedef std::vector<Node> node_vector;

  bool _do_evaluation;
  std::string _eval_filename;
  TTree* _eval_tree;
  int _ievent;
  double _vertex_z;

  // Eval variables
  int _trackid;
  short int _arm;
  double _x0reco;
  double _y0reco;
  double _z0reco;
  double _mxreco;
  double _myreco;
  double _chi2; // Chi2 (not reduced)
  short int _ndf; // Number of degrees of freedom = Nmeas - 4
  short int _nVtx;  // Number of VTX hits
  short int _nFvtx; // Number of FVTX hits
  int _size; // Total number of hits
  double _covar[4][4];
  double _covard[4];
  boost::array<double,20> _xhit;
  boost::array<double,20> _yhit;
  boost::array<double,20> _zhit;

  int _nresid; // number of residuals. SHOULD be the same as _size.
  boost::array<double,20> _dR;   // residual in R dir (at _zhit)
  boost::array<double,20> _dPhi; // residual in Phi dir (at _zhit)

  bool _use_svx_cluster;

  //! tracks
  TFvtxTrkMap* _trk_map; 
  
  //! coordinates
  TFvtxCoordMap* _coord_map; 
  
  //! hits
  TFvtxHitMap* _hit_map; 

  //! Track fit pars
  TFvtxStraightTrkParMap* _par_map;

  //! Hit residuals
  TFvtxResidualMap* _resid_map;

  // ! Set of nodes for the current track
  std::vector<double> _node_z;
  node_vector _rnodes;
  node_vector _phinodes;

  //! module timer
  PHTimeServer::timer _timer;		
  //PHTimer _timer;
};
