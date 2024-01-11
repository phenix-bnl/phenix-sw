#include <RecoverEmcHits.h>
#include "setIntflag.h"

#include <EmcHitMapEntry.h>
#include <EmcHitMap.h>
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

RecoverEmcHits::RecoverEmcHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumprecover.open("/phenix/scratch/pinkenbu/recoveremchits.dump");
#endif

  return;
}

int
RecoverEmcHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));


#ifdef useIntflag
  VariableArrayInt *emchit = findNode::getClass<VariableArrayInt>(topNode, "EmcHit_VarArray");
#else
  VariableArray *emchit = findNode::getClass<VariableArray>(topNode, "EmcHit_VarArray");
#endif

  EmcHitMap *emcmap;
  if (emchit)
    {
      emcmap  = findNode::getClass<EmcHitMap>(topNode, "EmcHit_comp");
      if (!emcmap)
        {
          emcmap = new EmcHitMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(emcmap, "EmcHit_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  //      emchit->identify();
  return EVENT_OK;
}

int
RecoverEmcHits::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "EmcHit_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "EmcHit_VarArray");
#endif

  if (hitarray)
    {
      EmcHitMap *emcmap  = findNode::getClass<EmcHitMap>(topNode, "EmcHit_comp");
      if (!emcmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate EmcHit_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif
      EmcHitMapEntry emcentry;
      while (size > 0)
        {
          emcentry.set_id(*array++);
          size--;
          emcentry.set_emcrawtdc(*array++);
          size--;
          emcentry.set_emcrawadc(*array++);
          size--;
          emcentry.set_emcrawadclg(*array++);
          size--;

          emcmap->AddHit(emcentry.get_id(), emcentry);
        }
#ifdef DUMP
      emcmap->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverEmcHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
