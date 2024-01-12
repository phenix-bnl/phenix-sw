#include "TrigReco.h"
#include <Fun4AllReturnCodes.h>

#include <Event.h>
#include <TrigLvl1v3.h>
#include <TriggerUtilities.h>

#include <getClass.h>
#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>

#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

static  TriggerUtilities tu;

TrigReco::TrigReco(const char *name): SubsysReco(name)
{
  return ;
}

int TrigReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int TrigReco::CreateNodeTree(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  /*
    set up header output node, currently we write the EventHeaderv1 class, the
    output node contains a PHObject
  */

  PHObjectNode_t *TrigLvl1Node = (PHObjectNode_t *)(iter.findFirst("PHIODataNode","TrigLvl1"));
  if(!TrigLvl1Node){
    TrigLvl1* triglvl1 = new TrigLvl1v3();
    TrigLvl1Node = new PHObjectNode_t(triglvl1,"TrigLvl1","PHObject"); // contain PHObject
    dstNode->addNode(TrigLvl1Node);
  }
  return 0;
}

int TrigReco::process_event(PHCompositeNode *topNode)
{
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
  TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if (!evt)
    {
      cout << PHWHERE << ": PRDF Node missing returning" << endl;
      return ABORTEVENT;
    }
  tu.fillTrigLvl1(evt, triglvl1);
  if (verbosity > 0)
    {
      triglvl1->identify();
    }
  return 0;
}

