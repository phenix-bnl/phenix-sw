#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"

#include "FclRawv2.h"
#include "FclOutv1.h"

#include "FclEvent.h"

#include "recoConsts.h"

#include "FcalReco.h"

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

FcalReco::FcalReco(const char *name)
{
  ThisName = name;
  mFclEvent = 0;
  return ;
}

FcalReco::~FcalReco()
{
  if (mFclEvent)
    {
      delete mFclEvent;
    }
}

int FcalReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int FcalReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  mFclEvent = new FclEvent();
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  mFclEvent->setCalibration(TimeStp); // set calibration by TimeStamp

  return 0;
}

int FcalReco::CreateNodeTree(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  FclRaw* fclRawNorth = new FclRawv2();
  PHObjectNode_t *fclRawNorthNode =
    new PHObjectNode_t(fclRawNorth, "fclRawNorth", "PHObject");

  FclRaw* fclRawSouth = new FclRawv2();
  PHObjectNode_t *fclRawSouthNode =
    new PHObjectNode_t(fclRawSouth, "fclRawSouth", "PHObject");

  dstNode->addNode(fclRawSouthNode);
  dstNode->addNode(fclRawNorthNode);

  FclOut* fclSouth = new FclOutv1();
  PHObjectNode_t *fclSouthNode =
    new PHObjectNode_t(fclSouth, "fclOutSouth", "PHObject");

  FclOut* fclNorth = new FclOutv1();
  PHObjectNode_t *fclNorthNode =
    new PHObjectNode_t(fclNorth, "fclOutNorth", "PHObject");

  dstNode->addNode(fclSouthNode);
  dstNode->addNode(fclNorthNode);

  return 0;
}

int FcalReco::process_event(PHCompositeNode *topNode)
{
  mFclEvent->event(topNode);
  return 0;
}
