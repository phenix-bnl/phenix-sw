//
// SpinReco 
//
//                                       2003/May/29
//                                       kaneta@bnl.gov
//                                       Masashi Kaneta, RBRC
//
//                                       2004/Dec/28 Modified by Hisa (htorii@bnl.gov)

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <Fun4AllReturnCodes.h>

#include <SpinEvent.h>
#include <SpinEventGL1p.h>
#include <SpinDataEventOut.h>
#include <SpinDataEventOutv2.h>
#include <SpinReco.h>

typedef PHIODataNode<SpinDataEventOut>  SpinDataEventOutNode_t;
typedef PHIODataNode<PHObject> PHObjectNode_t;

using namespace std;

SpinReco::SpinReco(const string &name):SubsysReco(name)
{
  pSpinEvent = 0;
  pSpinEventGL1p = 0;
  return ;
}

SpinReco::~SpinReco()
{
  delete pSpinEvent; 
  delete pSpinEventGL1p;
  return;
}

int
SpinReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int
SpinReco::InitRun(PHCompositeNode *topNode)
{
  pSpinEvent = new SpinEvent();
  pSpinEventGL1p = new SpinEventGL1p(topNode);
  return 0;
}

int
SpinReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  SpinDataEventOut* pSpinDataEventOut = new SpinDataEventOutv2();
  PHObjectNode_t *SpinDataEventOutNode = new PHObjectNode_t(pSpinDataEventOut,"SpinDataEventOut","PHObject");

  dstNode->addNode(SpinDataEventOutNode);

  return 0;
}

int 
SpinReco::process_event(PHCompositeNode *topNode)
{
  int iret = pSpinEvent->event(topNode);
  pSpinEventGL1p->event(topNode);
  if( iret == 0 )
    {
    return EVENT_OK;
    }
  else
    {
    return DISCARDEVENT;
    }
}

