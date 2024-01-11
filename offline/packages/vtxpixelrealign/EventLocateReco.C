#include "EventLocateReco.h"
#include "EventLocatev1.h"

#include <SvxPixelRawHitList.h>
#include <SvxStripRawHitList.h>
#include <SvxRawhitList.h>

#include <svxAddress.hh>

#include <PHIODataNode.h>
#include <PHCompositeNode.h>

#include <SyncObject.h>
#include <recoConsts.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <getClass.h>

#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;

static const char *SyncNodeName = "Sync";

EventLocateReco::EventLocateReco(const string &name): 
  SubsysReco(name),
  evtcnt(0),
  d_pixel(NULL),
  d_strip(NULL)
{
  return ;
}

int
EventLocateReco::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  recoConsts *rc = recoConsts::instance();
  PHNodeIterator iter(topNode);
  PHCompositeNode *runNode;
  runNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if (!runNode)
    {
      cout << PHWHERE << " RUN Node is missing doing nothing" << endl;
      return -1;
    }

  nodename << "EventLocation_" << rc->get_IntFlag("RUNNUMBER") << "_" << se->SegmentNumber();
  EventLocate *evtloc = findNode::getClass<EventLocate>(topNode,nodename.str());
  if (!evtloc)
    {
      evtloc  = new EventLocatev1();
      PHIODataNode <PHObject> *NewNode = new PHIODataNode <PHObject>(evtloc,nodename.str().c_str(), "PHObject"); // contains PHObject
      runNode->addNode(NewNode);
    }
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << " DST Node is missing doing nothing" << endl;
      return -1;
    }
  d_pixel = findNode::getClass<SvxPixelRawHitList>(topNode,"SvxPixelHits");
  if (! d_pixel)
  {
    d_pixel = new SvxPixelRawHitList();
    PHIODataNode <PHObject> *NewNode = new PHIODataNode <PHObject>(d_pixel,"SvxPixelHits", "PHObject"); // contains PHObject
    dstNode->addNode(NewNode);
  }
  d_strip = findNode::getClass<SvxStripRawHitList>(topNode,"SvxStripHits");
  if (! d_strip)
  {
    d_strip = new SvxStripRawHitList();
    PHIODataNode <PHObject> *NewNode = new PHIODataNode <PHObject>(d_strip,"SvxStripHits", "PHObject"); // contains PHObject
    dstNode->addNode(NewNode);
  }

  return EVENT_OK;
}

int 
EventLocateReco::process_event(PHCompositeNode *topNode)
{
  SyncObject *syncobject = findNode::getClass<SyncObject>(topNode, SyncNodeName);
  if (!syncobject)
    {
      cout << PHWHERE << " No Synchronisation Object" << endl;
      exit(1);
    }
  EventLocate *evtloc = findNode::getClass<EventLocate>(topNode,nodename.str());

  int evtseq = syncobject->EventNumber();
  if (evtseq < 0)
    {
      return ABORTEVENT;
    }
  evtloc->insert(evtseq,evtcnt);
  evtcnt++;

  svxAddress *address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if (!address)
  {
    std::cout << PHWHERE << "ERROR!! Can't find svxAddress!" << std::endl;
    return ABORTEVENT;
  }

  SvxRawhitList *d_rawhitlist = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  if (!d_rawhitlist)
  {
    std::cout << PHWHERE << "ERROR!! Can't find SvxRawhitList!" << std::endl;
    return ABORTEVENT;
  }
  for (int ihit = 0; ihit < d_rawhitlist->get_nRawhits(); ihit++)
  {
    d_pixel->add_hit(d_rawhitlist->get_Rawhit(ihit));
  }
  for (int ihit = 0; ihit < d_rawhitlist->get_nRawhits(); ihit++)
  {
    d_strip->add_hit(d_rawhitlist->get_Rawhit(ihit), address);
  }
  return EVENT_OK;
}

int
EventLocateReco::End(PHCompositeNode *topNode)
{
  EventLocate *evtloc = findNode::getClass<EventLocate>(topNode,nodename.str());
  evtloc->identify();
  return 0;
}
