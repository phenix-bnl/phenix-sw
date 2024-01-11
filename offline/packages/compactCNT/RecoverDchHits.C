#include <RecoverDchHits.h>
#include "setIntflag.h"

#include <DchHitMapEntry.h>
#include <DchHitMap.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>


#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <useInt.h>

#include <cstdlib>
#include <sstream>
#include <fstream>

using namespace std;

RecoverDchHits::RecoverDchHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoverdchhits.dump");
#endif

  return;
}

int
RecoverDchHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

#ifdef useIntflag
  VariableArrayInt *dchhit = findNode::getClass<VariableArrayInt>(topNode, "DchHit_VarArray");
#else
  VariableArray *dchhit = findNode::getClass<VariableArray>(topNode, "DchHit_VarArray");
#endif

  DchHitMap *dchmap;
  if (dchhit)
    {
      dchmap  = findNode::getClass<DchHitMap>(topNode, "DchHit_comp");
      if (!dchmap)
        {
          dchmap = new DchHitMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(dchmap, "DchHit_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  //      dchhit->identify();
  return EVENT_OK;
}

int
RecoverDchHits::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
  VariableArrayInt *hitarray;
  hitarray = findNode::getClass<VariableArrayInt>(topNode, "DchHit_VarArray");
#else
  VariableArray *hitarray;
  hitarray = findNode::getClass<VariableArray>(topNode, "DchHit_VarArray");
#endif
  
  if (hitarray)
    {
      DchHitMap *dchmap  = findNode::getClass<DchHitMap>(topNode, "DchHit_comp");
      if (!dchmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate DchHit_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();

#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif

      DchHitMapEntry dchentry;
      while (size > 0)
        {
          dchentry.set_id(*array++);
          size--;
          dchentry.set_arm(*array++);
          size--;
          dchentry.set_side(*array++);
          size--;
          dchentry.set_quality(*array++);
          size--;
	  dchentry.set_zed(useInt::GetFloat(*array++));
	  size--;
	  dchentry.set_phi(useInt::GetFloat(*array++));
	  size--;
	  dchentry.set_alpha(useInt::GetFloat(*array++));
	  size--;
	  dchentry.set_beta(useInt::GetFloat(*array++));
	  size--;
	  dchentry.set_phi0(useInt::GetFloat(*array++));
	  size--;
	  dchentry.set_theta0(useInt::GetFloat(*array++));
	  size--;
	  dchentry.set_momentum(useInt::GetFloat(*array++));
	  size--;
	  // nx1 and nx2 were added after Run 10 fast production, and the id was changed from 3000 to 3001
	  if(hitarray->Id() != 3000)
	    {
	      dchentry.set_nx1hits(*array++);
	      size--;
	      dchentry.set_nx2hits(*array++);
	      size--;
	    }
	  else
	    {
	      dchentry.set_nx1hits(-1);
              dchentry.set_nx2hits(-1);
	    }

          dchmap->AddHit(dchentry.get_id(), dchentry);
        }
#ifdef DUMP
      dchmap->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverDchHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
