#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <PHTimeStamp.h>
#include <lpcEvent.hh>
#include <lpcRawv2.h>
#include <recoConsts.h>
#include <lpcReco.h>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

lpcReco::lpcReco(const string &name): SubsysReco(name)
{
  mlpcEvent = 0;
  return ;
}

lpcReco::~lpcReco()
{
  delete mlpcEvent;
  return;
}

int
lpcReco::InitRun(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  //  PHCompositeNode* lpcNode = new PHCompositeNode("LPC");
  //topNode->addNode(lpcNode);

  //-* contains raw values (TDCs, ADCs, etc)
  lpcRaw *lpcraw = new lpcRawv2();
  PHObjectNode_t *lpcRawNode = new PHObjectNode_t(lpcraw, "lpcRaw", "PHObject");
  dstNode->addNode(lpcRawNode);

  //-* contains final physics products (energies, tof, etc)

  mlpcEvent = new lpcEvent();	// this module does all the reconstruction work
  mlpcEvent->setlpcRaw(lpcraw);
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  mlpcEvent->setCalibDataAll(TimeStp);
  return 0;
}

int
lpcReco::process_event(PHCompositeNode *topNode)
{
  mlpcEvent->setRawData(topNode);     // copy packet from PRDF to lpcRaw
  return 0;
}
