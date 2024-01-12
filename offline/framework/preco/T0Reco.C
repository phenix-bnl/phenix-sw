#include "T0Reco.h"
 
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>

#include <T0Outv3.h>
#include <BbcOut.h>
#include <ZdcOut.h>


#include <getClass.h>

#include <iostream>

using namespace std;

typedef PHIODataNode < PHObject > PHObjectNode_t;
typedef PHIODataNode <ZdcOut> ZdcOutNode_t;
typedef PHIODataNode <T0Out> T0OutNode_t;
typedef PHIODataNode <BbcOut> BbcOutNode_t;


T0Reco::T0Reco(const char *name): SubsysReco(name)
{
  return ;
}

int T0Reco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int T0Reco::CreateNodeTree(PHCompositeNode *topNode)
{

  /*
    set up t0 output node, currently we write the T0Outv1 class, the output
    node contains a PHObject
  */
  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  PHIODataNode<PHObject>* T0OutNode = (PHIODataNode<PHObject>*)(iter.findFirst("PHIODataNode", "T0Out"));
  if (!T0OutNode)
    {
      T0Out* t0out = new T0Outv3();
      T0OutNode = new PHIODataNode<PHObject>(t0out, "T0Out", "PHObject"); // contain PHObject
      dstNode->addNode(T0OutNode);
    }
  return 0;
}

int T0Reco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;
  float t0err;
  float t0;

  /*
    check if the node containing the T0Out object exists - note that here we don't
    need to create a versioned T0Out object (inheritance can be a pretty cool thing). 
  */

  T0Out* t0out = findNode::getClass<T0Out>(topNode, "T0Out");

  // dig into the bbc output and grab t0 if it is valid
  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout)
    {
      if (bbcout->isValid())  // check if BBC fired
        {
          t0 = bbcout->get_TimeZero();
          if (t0 > -999)
            {
              t0err = bbcout->get_dTimeZero();
              t0out->set_BbcT0(t0, t0err);
            }
        }
    }


  // ask the zdc for its opinion about a t0
  ZdcOut *zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");
  if (zdcout)
    {
      if (zdcout->isValid()) // check if zdc fired
        {
          t0 = zdcout->get_TimeZero();
          if (t0 > -800)
            {
              t0err = zdcout->get_TimeZeroError();
              t0out->set_ZdcT0(t0, t0err);
            }
        }
    }



  // Add a fake t0=0.0 to the t0out
  float faket0 = 0.0;
  t0err = 0.0;
  t0out->set_FkeT0(faket0, t0err);
  if (verbosity > 0)
    {
      t0out->identify();
    }
  return iret;
}

