
#include "TObject.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"

#include "RunHeader.h"
#include <cmath>

#include "NCCSimreco.h"
#include "NCCPisaHitv2.h"
#include "MPCEXABSPisaHitv1.h"
#include "MPCFPLTPisaHitv1.h"
#include "MPCEXENTPisaHitv1.h"
#include "NCCGetGEA.h"

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;


NCCSimreco::NCCSimreco(const char *name)
{
  ThisName = name;
}

int NCCSimreco::Init(PHCompositeNode *topNode)
{
  return 0;
}

int NCCSimreco::InitRun(PHCompositeNode *topNode)
{
  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }
    
  NCCPisaHit *NCCout = new NCCPisaHitv2();
  PHObjectNode_t *NCCOutNode = 
    new PHIODataNode<PHObject>(NCCout,"NCCPisaHit","PHObject");
  dstNode->addNode(NCCOutNode);

  MPCEXABSPisaHit *MPCEXABSout = new MPCEXABSPisaHitv1();
  PHObjectNode_t *MPCEXABSOutNode = 
    new PHIODataNode<PHObject>(MPCEXABSout,"MPCEXABSPisaHit","PHObject");
  dstNode->addNode(MPCEXABSOutNode);

  MPCFPLTPisaHit *MPCFPLTout = new MPCFPLTPisaHitv1();
  PHObjectNode_t *MPCFPLTOutNode = 
    new PHIODataNode<PHObject>(MPCFPLTout,"MPCFPLTPisaHit","PHObject");
  dstNode->addNode(MPCFPLTOutNode);

  MPCEXENTPisaHit *MPCEXENTout = new MPCEXENTPisaHitv1();
  PHObjectNode_t *MPCEXENTOutNode = 
    new PHIODataNode<PHObject>(MPCEXENTout,"MPCEXENTPisaHit","PHObject");
  dstNode->addNode(MPCEXENTOutNode);

  return 0;
}

int NCCSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  return 0;
}

int NCCSimreco::process_event(PHCompositeNode *topNode)
{

  NCCGetGEA(topNode); 
  return 0;
}

int NCCSimreco::ResetEvent(PHCompositeNode *topNode)
{
  //  Nothing in Main reco so nothing here...
  return 0;
}
