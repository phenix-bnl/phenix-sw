#include "PHIODataNode.h"
#include "PHCompositeNode.h"

#include "EventHeader.h"
#include "RunHeader.h"
#include "SyncObject.h"
#include "SyncObjectv2.h"

#include "Fun4AllReturnCodes.h"
#include "Fun4AllServer.h"
#include "recoConsts.h"
#include "SyncSimreco.h"

#include "getClass.h"
#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

static const char *SyncNodeName = "Sync";

SyncSimreco::SyncSimreco(const string &name): SubsysReco(name)
{
  return ;
}

int SyncSimreco::Init(PHCompositeNode *topNode)
{
  rc = recoConsts::instance();

  int iret = CreateNodeTree(topNode);
  return iret;
}

int SyncSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << " DST Node is missing doing nothing" << endl;
      return -1;
    }


  SyncObject* syncobject = new SyncObjectv2();
  PHObjectNode_t *SyncObjectNode = new PHObjectNode_t(syncobject, SyncNodeName, "PHObject"); // contain PHObject
  dstNode->addNode(SyncObjectNode);

  return EVENT_OK;
}

int SyncSimreco::process_event(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  SyncObject *syncobject = findNode::getClass<SyncObject>(topNode, SyncNodeName);
  if (!syncobject)
    {
      cout << PHWHERE << " No Synchronisation Object, no parallel reading of multiple inputs" << endl;
      return ABORTEVENT;
    }

  syncobject->EventCounter(se->DstEvents());

  syncobject->EventNumber( se->DstEvents() + 1 );

  // for simulation objects, you set the runnumber using the recoConsts
  syncobject->RunNumber( rc->get_IntFlag("RUNNUMBER",0) );

  // simulation segments are always 0
  syncobject->SegmentNumber(se->SegmentNumber());

  if (verbosity > 0)
    {
      syncobject->identify();
    }

  return EVENT_OK;
}

