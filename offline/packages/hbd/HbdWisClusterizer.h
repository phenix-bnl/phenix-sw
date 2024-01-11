#ifndef __HBDWISCLUSTERIZER__
#define __HBDWISCLUSTERIZER__

#include <SubsysReco.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <string>
#include <vector>
#include <hbdDetectorGeo.hh>
#include <HbdWisClusterList.h>
#include <HbdFinalSimSupport.h>


typedef PHIODataNode <HbdWisClusterList> HbdClusterListNode_t;


class PHCompositeNode;
class HbdCellList;
class HbdBlobList;
class HbdFinalSimSupport;

class HbdWisClusterizer: public SubsysReco
{
 public:
  HbdWisClusterizer(const char *NodeName = "HbdCellList",
		    const char *CNTName  = "PHCentralTrack");

  virtual ~HbdWisClusterizer(){}

  int Init         (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void setNodeName(const std::string &nodename) {d_CNTName = nodename;}

 protected:

  float pad_area[192];
  int CellIndex[192][12];
  int CellInClstIndex[600][10];
  int cluster_used[600];
  int CellInClstIndexNew[150][30];
  int cell_index_used[600][10];
  float cellcharge[2304];
  int cellsector[2304];
  int cellpadnum[2304];
  int runnum;
  bool isSimulation, isEmbedding, isHijing;
  bool Run9_pp;
  bool Run10_AuAu;
  bool MERGE;      
  bool USE_COG;

  //
  // DataNodes for the HBD ...
  //
    
  HbdClusterList *ClusterList;
  HbdCellList *CellList;
  HbdBlobList *BlobList;
  HbdWisClusterList * d_clust_list;
    HbdClusterListNode_t *clust_node;


  HbdFinalSimSupport simsupport;
  hbdDetectorGeo hbdgeo;

  //
  // Output node name;
  //
  std::string d_NodeName;   // node for real
  std::string d_CNTName;    // node name for CNT or EWG or cEWG
};

struct PadInCluster{
  int index;
  float charge;
  float positionY;
  float positionZ;
  int sector;
};

bool sort_by_charge(PadInCluster const& first, PadInCluster const& second);
std::vector <float> hbd_offsets_run9(unsigned int sector, float z);

#endif

