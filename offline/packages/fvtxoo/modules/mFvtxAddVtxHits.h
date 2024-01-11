#ifndef _MFVTXADDVTXHITS_H_
#define _MFVTXADDVTXHITS_H_

#include <PHTimer.h>
#include <mFvtxModuleBase.h>
#include <TRandom3.h>
#include <vector>
#include <SvxCluster.h>

#include <mFvtxModuleBase.h>

class TTree;
class PHCompositeNode;
class TMutMCTrkMap;
class TFvtxTrkMap;
class TFvtxCoordMap;
class TFvtxMCHitMap;
class TFvtxPisaHitMap;
class TFvtxSvxClusterMap;

class mFvtxAddVtxHits : public mFvtxModuleBase
{
 public:
  mFvtxAddVtxHits();
  ~mFvtxAddVtxHits();
  void init(PHCompositeNode*);
  void init_run(PHCompositeNode*){}; 
  PHBoolean event(PHCompositeNode* topNode);
  void end(PHCompositeNode*);
  void set_file_name(std::string name) { _evalFilename = name; }
  void set_use_svx_cluster(bool flag) { _use_svx_cluster = flag;}
  bool get_use_svx_cluster() { return _use_svx_cluster; }

 private:

  void set_interface_ptrs(PHCompositeNode* topNode);
  void book_trees();
  void clear_track();
  void find_tracks();
  double getResiduals();
  int get_VtxLayerNumber(const float r);
  void get_rotated_fits();
  //  bool passedZcut(double r_hit, double z_hit);

  TMutMCTrkMap* _mut_mc_trk_map;
  TFvtxTrkMap* _trk_map;
  TFvtxCoordMap* _coord_map;
  TFvtxMCHitMap* _mc_hit_map;
  TFvtxPisaHitMap* _pisa_hit_map;
  TFvtxSvxClusterMap* _clus_map;

  bool _use_svx_cluster;
  bool _do_evaluation;

  TRandom3 _rnd;

  std::string _evalFilename;
  double _dPhi;
  double _dZcut;
  int _ievent;

  std::vector<double> _r_resid;
  std::vector<double> _rmcv;
  std::vector<double> _xmcv;
  std::vector<double> _ymcv;
  std::vector<double> _zmcv;
  std::vector<double> _phimcv;
  std::vector<double> _rv;
  std::vector<double> _xv;
  std::vector<double> _yv;
  std::vector<double> _zv;
  std::vector<double> _phiv;

  std::vector<double> _in;
  std::vector<double> _out;
  std::vector<double> _x_begin;
  std::vector<double> _y_begin;
  std::vector<double> _x_end;
  std::vector<double> _y_end;
  std::vector<double> _inw;
  std::vector<double> _outw;

  std::vector<double> _rwv;
  std::vector<double> _xwv;
  std::vector<double> _ywv;
  std::vector<double> _zwv;

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  enum { max_size = 50 };
  TTree* _trk_ab_tree;
  int _track;
  int _size;
  float _rchi2;
  float _rmc_slopeFit;
  float _rmc_offsetFit;
  float _phimc_slopeFit;
  float _phimc_offsetFit;
  float _xmc_int;
  float _ymc_int;
  float _xmc_slope;
  float _ymc_slope;
  float _r_slopeFit;
  float _r_offsetFit;
  float _phi_slopeFit;
  float _phi_offsetFit;
  float _x_int;
  float _y_int;
  float _x_slope;
  float _y_slope;
  float _phi_rot;
  float _inPlane_int;
  float _outPlane_int;
  float _inPlane_slope;
  float _outPlane_slope;
  bool _foundVtxHit;
  bool _hasCharmParent[max_size];
  bool _hasBottomParent[max_size];
  int _mc_tracknum[max_size];
  int _mc_hitnum[max_size];
  int _coordnum[max_size];
  int _pid[max_size];
  int _parent_pid[max_size];
  int _halfWedgeId[max_size];
  int _nstrips[max_size];
  float _xmc[max_size];
  float _ymc[max_size];
  float _zmc[max_size];
  float _rmc[max_size];
  float _phimc[max_size];
  float _x[max_size];
  float _y[max_size];
  float _z[max_size];
  float _r[max_size];
  float _phi[max_size];
  float _inPlane[max_size];
  float _outPlane[max_size];
  float _x_cluster_width[max_size];
  float _y_cluster_width[max_size];
  float _x_cluster_begin[max_size];
  float _y_cluster_begin[max_size];
  float _x_cluster_end[max_size];
  float _y_cluster_end[max_size];

  //! module timer
  PHTimer _timer;
};

#endif /* _MFVTXADDVTXHITS_H_*/
