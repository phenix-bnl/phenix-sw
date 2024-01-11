#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <PHTimeStamp.h>
#include <LPolEvent.hh>
#include <LPolRawv1.h>
#include <recoConsts.h>
#include <LPolReco.h>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

LPolReco::LPolReco(const string &name): SubsysReco(name)
{
  mlpolEvent = 0;
  return ;
}

LPolReco::~LPolReco()
{
  delete mlpolEvent;
  return;
}

int
LPolReco::InitRun(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  //  PHCompositeNode* LPolNode = new PHCompositeNode("LPOL");
  //topNode->addNode(LPolNode);

  //-* contains raw values (TDCs, ADCs, etc)
  LPolRaw *lpolraw = new LPolRawv1();
  PHObjectNode_t *lpolRawNode = new PHObjectNode_t(lpolraw, "LPolRaw", "PHObject");
  dstNode->addNode(lpolRawNode);

  //-* contains final physics products (energies, tof, etc)

  mlpolEvent = new LPolEvent();	// this module does all the reconstruction work
  mlpolEvent->setLPolRaw(lpolraw);
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  mlpolEvent->setCalibDataAll(TimeStp);
  return 0;
}

int
LPolReco::process_event(PHCompositeNode *topNode)
{
  mlpolEvent->setRawData(topNode);     // copy packet from PRDF to LPolRaw
  return 0;
}
