#ifndef __KFDEBUGTREE_H_
#define __KFDEBUGTREE_H_

//----------------------------------------------
// This is just a utility class for writing out interesting information.
//

class TTree;
class TFile;
class PHSnglCentralTrack;
class McEvalSingleList;
class PHCentralTrack;

#include <boost/multi_array.hpp>

class KFDebugTree
{
 public:
  KFDebugTree(int layers, std::string outfilename);
  ~KFDebugTree();

  typedef boost::multi_array<int, 1> array1di;
  typedef boost::multi_array<float, 1> array1d;
  typedef boost::multi_array<float, 2> array2d;
  typedef boost::multi_array<float, 3> array3d;
  typedef boost::multi_array<float, 4> array4d;

  void Fill();
  void Finish();
  void reset();

  void fill_dc_map(McEvalSingleList*, PHCentralTrack*);
  void fill_dc_info(const PHSnglCentralTrack*);

  bool is_filling_tree() {return make_tree_;}

  TFile* outfile_;
  TTree* tree_;
  bool make_tree_;

  int nlayers;
  array2d kfmom;
  array2d initmom;
  array2d mcmom;
  float mcvtxmom[3];
  float mcvtxpos[3];
  array1di mcpid;
  int nsegments;
  int nclusters;
  array1d chisq;
  int ndf;
  float initcharge;
  float kfcharge;
  array2d mcpos;
  array2d clusterpos;
  array3d state;
  array4d covar;
  array2d statemc;
  float vtx[3];
  float pca[3];

  float dcmom;
  float dcphi;
  float dctheta;
  float dcalpha;
  float simdcmom;
  float simdcphi;
  float simdctheta;
  float simdcalpha;

  std::map<const PHSnglCentralTrack*, unsigned int> _dch_reco_mc_map; 
  McEvalSingleList* _mc_eval_list;
};

#endif // __KFDEBUGTREE_H_
