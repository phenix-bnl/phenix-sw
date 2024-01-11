#include "HBDMiniCellReset.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include "Fun4AllReturnCodes.h"

#include "PHInclusiveNanoCuts.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "HbdMiniCellList.h"

#include "getClass.h"

#include <cassert>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHCentralTrack> PHParticleNode_t;

HBDMiniCellReset::HBDMiniCellReset(): SubsysReco("HBD")
{
  return;
}
                                                                                                                            
HBDMiniCellReset::~HBDMiniCellReset()
{
  return ;
}

//////////////////////////////////////Init Run//////////////////////////////////////////////////

int HBDMiniCellReset::InitRun(PHCompositeNode *topNode)
{
  return 0;
}


///////////////////////////process event////////////////////////////////////////////////////

int HBDMiniCellReset::process_event(PHCompositeNode *topNode)
{
  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "EWGCentralTrack");
  if (!cnt)
    {
      cout << PHWHERE << "Whoops, did not find EWGCentralTrack! Return and do nothing. " << endl;
      return 0;
    }
  
  if (cnt->get_npart() == 0)
    {
      HbdMiniCellList *Hbd = findNode::getClass<HbdMiniCellList>(topNode,"HbdMiniCellList");
      if (Hbd)
        {
          Hbd->Reset();
        }
    }
  
  return 0;
}
////////////////////ResetEvent//////////////////////////////////////////////////////

int HBDMiniCellReset::ResetEvent(PHCompositeNode *topNode)
{
  // We are resetting just prior to refilling...
  return 0;
}


