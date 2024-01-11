#include <RecoverAccHits.h>
#include "setIntflag.h"

#include <AccHitMapEntry.h>
#include <AccHitMap.h>
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

RecoverAccHits::RecoverAccHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoveracchits.dump");
#endif

  return;
}

int
RecoverAccHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
#ifdef useIntflag
  VariableArrayInt *acchit = findNode::getClass<VariableArrayInt>(topNode, "AccHit_VarArray");
#else
  VariableArray *acchit = findNode::getClass<VariableArray>(topNode, "AccHit_VarArray");
#endif

  AccHitMap *accmap;
  if (acchit)
    {
      accmap  = findNode::getClass<AccHitMap>(topNode, "AccHit_comp");
      if (!accmap)
        {
          accmap = new AccHitMap();
	  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(accmap, "AccHit_comp" , "PHObject");

          dstNode->addNode(PHObjectIONode);
        }
      //      acchit->identify();
    }

  return EVENT_OK;
}

int
RecoverAccHits::process_event(PHCompositeNode *topNode)
{
  int irecover=0;

#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "AccHit_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "AccHit_VarArray");
#endif

  if (hitarray)
    {
      AccHitMap *accmap  = findNode::getClass<AccHitMap>(topNode, "AccHit_comp");
      if (!accmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate AccHit_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif
      AccHitMapEntry accentry;
      while (size > 0)
        {
          accentry.set_hitid(*array++);
          size--;
          accentry.set_hitconfig(*array++);
          size--;
	  for(int ibox=0;ibox<4;ibox++)
	    {
	      accentry.set_ph1(ibox, useInt::GetFloat(*array++));
	      size--;
	      accentry.set_ph2(ibox, useInt::GetFloat(*array++));
	      size--;
	      accentry.set_t1(ibox, useInt::GetFloat(*array++));
	      size--;
	      accentry.set_t2(ibox, useInt::GetFloat(*array++));
	      size--;
	    }
          accmap->AddHit(irecover, accentry);
	  irecover++;
        }
#ifdef DUMP
      accmap->identify(dumprecover);
#endif
    }

  return EVENT_OK;
}

int
RecoverAccHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
