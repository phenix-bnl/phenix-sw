#ifndef __HBDCLUSTERIZER_H__
# define __HBDCLUSTERIZER_H__

#include <SubsysReco.h>
#include <PHCompositeNode.h>
#include <getClass.h>

#include <map>
#include <vector>

#include "hbdDetectorGeo.hh"

#include "HbdCellList.h"
#include "HbdCell.h"

class PHCompositeNode;
class HbdClusUtil; 
class HbdCellList;
class HbdBlobList;
class HbdGhitList;



//
// Global definitions
//

#define CDIFF 1.1
#define ClustThreshold 10
#define HighCellThreshold 8

class HbdClusterizer : public SubsysReco
{
  
  public:
  
  HbdClusterizer();
  virtual ~HbdClusterizer();
  int Init(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  //   int Clusterizer(HbdCellList *CellList, HbdBlobList *BlobList, HbdGhitList *GhitList);
  //   int Clusterizer(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void cluster(HbdCellList *CellList);
  void split();
  int fill(HbdBlobList *BlobList);
  void SetChargeThreshold(float threshold){ ChargeThreshold = threshold; };
  float SetChargeThreshold(){ return ChargeThreshold; };
  
  protected:
  //int BlobClustering(HbdCellList*, HbdClusUtil**&, int&);
  //int FillHbdBlob(HbdClusUtil**, HbdGhitList*, HbdBlobList*, int);
  int FillGEANT(HbdGhitList*);
  void merge(int cid1, int cid2);
  void findlocalmaxima();
  bool isNeighbour(int sect1, int pad1, int sect2, int pad2);
  std::map<int,HbdClusUtil>  clusters;
  
  // Node
  HbdCellList *CellList;
  HbdBlobList *BlobList;
  HbdGhitList *GhitList;
  
  private:
  
  //! geometry
  /*! 
  a reference is used in place of a pointer so that the default constructor
  is called in the parent object constructor, and the object is automatically 
  deleted when the parent object is
  */
  hbdDetectorGeo hbdgeo;
  
  float ChargeThreshold;
  
};


//
// Utility class to define cluster from cells
//
class HbdClusUtil{
  public:
  std::vector<HbdCell*> cluster_cells;
  // Below are used just at the final stage.
  
  int get_ncells(){return cluster_cells.size();};
  void add_cell(HbdCell *newcell, double weight=1.0){
    cluster_cells.push_back(newcell);
    newcell->set_clusterid(clusterid);
    TotChg+=(newcell->get_charge())*weight;
  };
  void set_clusterid(int clusid){
    clusterid=clusid;
  };
  float get_charge(){
    return TotChg;
  }
  
  int parent_clusterid;
  std::map<int,HbdCell*> local_max;
  std::vector<int> daughterclusters;
  
  float TotChg;                  // Sum of Charge of cells in this cluster 
  float PosY, PosZ;              // Center-Of-gravity Position (Local)
  float RmsY, RmsZ;              // Rms of Center-Of-gravity Position (Local)
  int clusterid;
  
  HbdClusUtil() {
    clusterid=-1;
    cluster_cells.clear();
    daughterclusters.clear();
    TotChg=0.0;
    PosY=PosZ=RmsY=RmsZ=0.0; 
    parent_clusterid=-1;
    local_max.clear();
  }
  void print(){
    std::vector<HbdCell*>::iterator ic;
    for(ic=cluster_cells.begin();ic!=cluster_cells.end(); ++ic){
      std::cout << " padnumber " << (*ic)->get_padnum() << std::endl;
      std::cout << " charge " << (*ic)->get_charge() << std::endl;
      std::cout << std::endl;
    }
  }
};

#endif /* __HBDCLUSTERIZER_H__ */

