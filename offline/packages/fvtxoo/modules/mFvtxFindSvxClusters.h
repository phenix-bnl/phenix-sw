#ifndef __MFVTXFINDSVXCLUSTERS_HH__
#define __MFVTXFINDSVXCLUSTERS_HH__

#include <TFvtxSvxClusterMap.h>
#include <TFvtxHitMap.h>
#include <PHTimeServer.h>
#include <mFvtxFindSvxClustersPar.h>
#include <FvtxGeom.h>
#include <mFvtxModuleBase.h>

class TTree;
class PHCompositeNode;
class SvxClusterList;

class mFvtxFindSvxClusters : public mFvtxModuleBase
{
public:

  //constructor
  mFvtxFindSvxClusters();

  //destructor
  ~mFvtxFindSvxClusters() {}

  void init(PHCompositeNode*);
  void init_run(PHCompositeNode*);
  void end(PHCompositeNode*);

  //event method	
  PHBoolean event(PHCompositeNode*);

private:
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  void init_trees();
  void fill_trees();

  //gets local pointer to useful nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  void initialize_event();
  void merge_clusters(std::vector<SvxCluster*>& clus, std::list<SvxCluster*>& merged);

  //find all clusters
  void find_clusters();

  //module parameters
  const mFvtxFindSvxClustersPar* _mod_par;

  SvxClusterList* _svxclus_node;

  std::vector<SvxCluster*> _svxClusters[4]; // Separated by layer

  // cluster map node
  TFvtxSvxClusterMap* _clus_map;		

  //module timer
  PHTimeServer::timer _timer;

  // eval variables
  TTree* _eval_tree;

  int _ievent;
  short _layer;
  short _sensor;
  short _ladder;
  float _x;
  float _y;
  float _z;
  float _xerr;
  float _yerr;
  float _zerr;

};

#endif /* __MFVTXFINDCLUS_HH__ */
