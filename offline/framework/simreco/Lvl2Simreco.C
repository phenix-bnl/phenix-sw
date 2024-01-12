#include "Lvl2Simreco.h"
#include "Lvl2Event.h"
#include "Lvl2OutArrayv1.h"
#include "Lvl2DecisionOutv1.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"


#include <cmath>

using namespace std;

Lvl2Simreco::Lvl2Simreco(const string &name): SubsysReco(name)
{
  mLvl2Event = 0;
  return;
}

Lvl2Simreco::~Lvl2Simreco()
{
  if (mLvl2Event)
    {
      delete mLvl2Event;
    }
  return;
}

int Lvl2Simreco::InitRun(PHCompositeNode *topNode)
{
  // find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  // we want to run level 2 triggers
  
  // Now initialize level 2 with triggers turned on
  mLvl2Event=new Lvl2Event(True);

  // Lvl2 decision node
  Lvl2DecisionOut *decisionOut = new Lvl2DecisionOutv1();
  PHIODataNode<PHObject>* Lvl2DecisionNode =
    new PHIODataNode<PHObject>(decisionOut,"L2Decision","PHObject");
  dstNode->addNode(Lvl2DecisionNode);

  // Lvl2 array node
  cout << "instantiating Lvl2OutArrayv1" << endl;
  Lvl2OutArray *lvl2out = new Lvl2OutArrayv1();
  PHIODataNode<PHObject>* Lvl2OutNode =
    new PHIODataNode<PHObject>(lvl2out,"Lvl2OutArray","PHObject");
  dstNode->addNode(Lvl2OutNode);
  
  return 0;
}

int 
Lvl2Simreco::process_event(PHCompositeNode *topNode)
{
  mLvl2Event->RunL2TriggersPISAToDST(topNode);

  return 0;
}

