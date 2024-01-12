#include <iostream>
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "MuPCSimreco.h"

#include "phool.h"

#include "uIDLL1Road.h"
#include "MuPCCluster.h"
#include "uIDLL1Roadv1.h"
#include "MuPCClusterv1.h"
#include "MuPCAnalysis.h"
#include "uIDLL1Analysis.h"
#include "mupcghitWrapper.h"
#include "MuPCGetGEA.h"

using namespace std;

typedef PHIODataNode <PHObject>   PHObjectNode_t;
typedef PHIODataNode <uIDLL1Road> uIDLL1RoadNode_t;
typedef PHIODataNode <MuPCCluster> MuPCClusterNode_t;
typedef PHIODataNode <PHTable> PHTable_t;

MuPCSimreco::MuPCSimreco(int v): SubsysReco("MuPC")
{
  version = v;

  mupc1ghit = NULL;
  mupc2ghit = NULL;
  mupc3ghit = NULL;

  return ;
}

int MuPCSimreco::Init(PHCompositeNode *topNode)
{
  CreateNodeTree(topNode);

  // Make an MuPCAnalysis module.
  uidll1analysis = new uIDLL1Analysis();
  mupcAnalysis = new MuPCAnalysis(); 

  uidll1analysis->Init(topNode);
  mupcAnalysis->Init(topNode);

  return 0;
}

int MuPCSimreco::InitRun(PHCompositeNode *topNode)
{

  uidll1analysis->InitRun(topNode);
  mupcAnalysis->InitRun(topNode);

  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  PHNodeIterator itergea(dstNode);
  PHCompositeNode *geaNode;
  geaNode = dynamic_cast<PHCompositeNode*>(itergea.findFirst("PHCompositeNode","GEA"));
  if (!dstNode || !geaNode)
    {
      cout << PHWHERE << " Could not find DST or GEA node" << endl;
      return -1;
    }
  // Make nodes containing the objects...
  PHTable_t* mupc1ghitNode = new PHIODataNode<PHTable>(mupc1ghit, "mupc1ghit");
  PHTable_t* mupc2ghitNode = new PHIODataNode<PHTable>(mupc2ghit, "mupc2ghit");
  PHTable_t* mupc3ghitNode = new PHIODataNode<PHTable>(mupc3ghit, "mupc3ghit");
  geaNode->addNode(mupc1ghitNode);
  geaNode->addNode(mupc2ghitNode);
  geaNode->addNode(mupc3ghitNode);

  return 0;
}

int MuPCSimreco::ResetEvent(PHCompositeNode *topNode)
{
  uidll1road->Reset();
  mupc1cluster->Reset();
  mupc2cluster->Reset();
  mupc3cluster->Reset();

  return 0;
}

int MuPCSimreco::process_event(PHCompositeNode *topNode)
{
  MuPCGetGEA(topNode);
  uidll1analysis->Event(topNode);
  mupcAnalysis->Event(topNode);
  return 0;
}

bool MuPCSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  // Find the DST node so we can put objects there...
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  if (!dstNode)
    {
      cout << PHWHERE << " Could not find DST or GEA node" << endl;
      return -1;
    }
  cout << "Initialize the Muon Trigger upgrade Detector Analysis..." << endl;

  // Make the "data" objects.
  // NOTE:  This is the *only* place in *any* code that refers to the version...
  size_t mr = 1500;
  switch (version)
    {
    case 1:
      uidll1road  = new uIDLL1Roadv1();
      mupc1cluster  = new MuPCClusterv1();
      mupc2cluster  = new MuPCClusterv1();
      mupc3cluster  = new MuPCClusterv1();
      mupc1ghit = new mupcghitWrapper("mupc1ghit", mr);
      mupc2ghit = new mupcghitWrapper("mupc2ghit", mr);
      mupc3ghit = new mupcghitWrapper("mupc3ghit", mr);
      break;

    default:
      cout << PHWHERE << "WARNING::MuPCSimreco...Unknown Version Requested: use version 1 now " << version <<endl;
      uidll1road  = new uIDLL1Roadv1;
      mupc1cluster  = new MuPCClusterv1;
      mupc2cluster  = new MuPCClusterv1;
      mupc3cluster  = new MuPCClusterv1;
      mupc1ghit = new mupcghitWrapper("mupc1ghit", mr);
      mupc2ghit = new mupcghitWrapper("mupc2ghit", mr);
      mupc3ghit = new mupcghitWrapper("mupc3ghit", mr);
      break;

    }

  // Make nodes containing the objects...
  uIDLL1RoadNode_t *road = new PHIODataNode<uIDLL1Road>(uidll1road,"uIDLL1Road","PHObject");
  MuPCClusterNode_t *cluster1 = new PHIODataNode<MuPCCluster>(mupc1cluster,"MuPC1Cluster","PHObject");
  MuPCClusterNode_t *cluster2 = new PHIODataNode<MuPCCluster>(mupc2cluster,"MuPC2Cluster","PHObject");
  MuPCClusterNode_t *cluster3 = new PHIODataNode<MuPCCluster>(mupc3cluster,"MuPC3Cluster","PHObject");

  // Put the nodes in the tree below "DST"...
  dstNode->addNode(road);
  dstNode->addNode(cluster1);
  dstNode->addNode(cluster2);
  dstNode->addNode(cluster3);

  
 return True;
}
