#ifndef __MPCCLUSTERIDRECO_H__
#define __MPCCLUSTERIDRECO_H__


#include <SubsysReco.h>
#include <vector>

class PHCompositeNode;
class MpcMap;
class mpcClusterContent;
class mpcClusterContainer;
class mpcGeaClusterContainer;


/*
November 6, 2010
Beau Meredith

This is a class which uses the output of MpcGeaClusterReco (used to
identify particle energy depositions into a cluster) and puts the
relevant information into the mpcClusterContent object.  As for now,
it puts up to 4 particles but in reality there is no limit.  It stores
the energy fraction of the particle, energy deposited by the particle,
itorigin, kfcode,pythia id, parent kf, and parent id.  4 ptcls is used
as a cutoff b/c of a pragmatic reason: sometimes the energy sharing
process can put very small fractions of a ptcls energy into a cluster
where it does not belong.  There is no reason to include these
particles and they are a mere nuissance.

This class can identify particle compositions of clusters across
multiple embedded nodes (each of which must have a MpcGeaClusterReco
module registered).  To do multiple nodes, you just need to specify
the number of nodes and make sure you have properly registered the
MpcGeaClusterReco objects.


*/




class MpcClusterIDReco: public SubsysReco
{
public:
  MpcClusterIDReco(const std::string &name = "MpcClusterIDReco",int nnodes=1);
  virtual ~MpcClusterIDReco();

  //int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);
  void Print(const std::string & = "ALL") const;

  void FillClusters();
  int GetType(mpcClusterContent* clus);
  int GetType2(mpcClusterContent* clus);
private:
  MpcMap *mpcmap;

  mpcClusterContainer *mpcclus;
  mpcGeaClusterContainer *mpcgeaclus;

  std::vector< mpcGeaClusterContainer* > mpcgeaclus_vector;
  
  int n_nodes;

  
};

#endif /* __MPCCLUSTERIDRECO_H__ */

