#include "SyncReco.h"

#include <PHIODataNode.h>
#include <PHCompositeNode.h>

#include <EventHeader.h>
#include <RunHeader.h>
#include <SyncObject.h>
#include <SyncObjectv2.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <getClass.h>
#include <iostream>

using namespace std;

static const char *SyncNodeName = "Sync";

SyncReco::SyncReco(const string &name): SubsysReco(name)
{
  return ;
}

int SyncReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int SyncReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << " DST Node is missing doing nothing" << endl;
      return -1;
    }


  SyncObject *syncobject = findNode::getClass<SyncObject>(topNode, SyncNodeName);
  if (!syncobject)
    {
      syncobject = new SyncObjectv2();
      PHIODataNode <PHObject> *SyncObjectNode = new PHIODataNode <PHObject>(syncobject, SyncNodeName, "PHObject"); // contains PHObject
      dstNode->addNode(SyncObjectNode);
    }

  return EVENT_OK;
}

int SyncReco::process_event(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  SyncObject *syncobject = findNode::getClass<SyncObject>(topNode, SyncNodeName);
  if (!syncobject)
    {
      cout << PHWHERE << " No Synchronisation Object, no parallel reading of multiple inputs" << endl;
      return ABORTEVENT;
    }

  syncobject->EventCounter(se->PrdfEvents());

  EventHeader* eventheader = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (eventheader)
    {
      syncobject->EventNumber(eventheader->get_EvtSequence());
    }

  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader)
    {
      syncobject->RunNumber(runheader->get_RunNumber());
    }
  syncobject->SegmentNumber(se->SegmentNumber());
  if (verbosity > 0)
    {
      syncobject->identify();
    }
  return EVENT_OK;
}

