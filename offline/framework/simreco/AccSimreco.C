#include "AccSimreco.h"

#include "AccHitv1.h"
#include "AccRawv2.h"
#include "AccGeometry.h"
#include "AerGeaHitsv1.h"
#include "AerGetGEA.h"
#include "AccSnglRaw.h"
#include "AccSnglHit.h"

#include "RunHeader.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"

#include "getClass.h"

#include <cmath>

typedef PHIODataNode<AccRaw>      AccRawNode_t;
typedef PHIODataNode<AccHit>      AccHitNode_t;
typedef PHIODataNode<AerGeaHits>  AerGeaHitsNode_t;
typedef PHDataNode <AccGeometry>  AccGeometryNode_t;

using namespace std;

//____________________________________________________________
AccSimreco::AccSimreco(const string &name): SubsysReco(name)
{
  d_geo = 0;
  d_raw = 0;
  d_hit = 0;
  return;
}

//______________________________________________________
AccSimreco::~AccSimreco()
{}

//______________________________________________________
int AccSimreco::InitRun(PHCompositeNode *topNode)
{
  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));

  // Setup your code here.  This is called only for the first event.
  PHCompositeNode* accNode = new PHCompositeNode("ACC");
  topNode->addNode(accNode);

  d_raw = new AccRawv2();
  d_hit = new AccHitv1();
  d_gea = new AerGeaHitsv1();
  d_geo = new AccGeometry();

  AccRawNode_t *raw = new AccRawNode_t(d_raw, "AccRaw", "PHObject");
  dstNode->addNode(raw);

  AccHitNode_t *hit = new AccHitNode_t(d_hit, "AccHit", "PHObject");
  dstNode->addNode(hit);

  AerGeaHitsNode_t *gea = new AerGeaHitsNode_t(d_gea,"AerGeaHits","PHObject");
  PHCompositeNode *geaNode =
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  geaNode->addNode(gea);

  AccGeometryNode_t *geo = new AccGeometryNode_t(d_geo,"AccGeometry");
  parNode->addNode(geo);

  RunHeader* d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");

  if (d_runhdr)
  {
    int runNumber = d_runhdr->get_RunNumber();
    d_geo->fetch(runNumber);
  }
  
  return 0;
}

int 
AccSimreco::process_event(PHCompositeNode *topNode)
{
  
  // Here your event by event routines are called.
  AerGetGEA(topNode);
  d_acc.GeaToRaw(topNode);
  d_acc.RawToRec(topNode, d_geo);
  
  return 0;
}
