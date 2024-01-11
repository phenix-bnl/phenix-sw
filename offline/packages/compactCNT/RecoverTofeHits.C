#include <RecoverTofeHits.h>
#include "setIntflag.h"

#include <TofeHitMapEntry.h>
#include <TofeHitMap.h>
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

RecoverTofeHits::RecoverTofeHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recovertofehits.dump");
#endif

  return;
}

int
RecoverTofeHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

#ifdef useIntflag
  VariableArrayInt *tofehit = findNode::getClass<VariableArrayInt>(topNode, "TofeHit_VarArray");
#else
  VariableArray *tofehit = findNode::getClass<VariableArray>(topNode, "TofeHit_VarArray");
#endif

  TofeHitMap *tofemap;
  if (tofehit)
    {
      tofemap  = findNode::getClass<TofeHitMap>(topNode, "TofeHit_comp");
      if (!tofemap)
        {
          tofemap = new TofeHitMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(tofemap, "TofeHit_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  //      tofehit->identify();
  return EVENT_OK;
}

int
RecoverTofeHits::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "TofeHit_VarArray");
  const int *array;
  if (hitarray)
    array = hitarray->get_array();
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "TofeHit_VarArray");
  const short int *array;
  if (hitarray)
    array = hitarray->get_array();
#endif

  if (hitarray)
    {
      TofeHitMap *tofemap  = findNode::getClass<TofeHitMap>(topNode, "TofeHit_comp");
      if (!tofemap)
        {
          cout << PHWHERE << "Fatal: Cannot locate TofeHit_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();

      TofeHitMapEntry tofeentry;
      while (size > 0)
        {
          tofeentry.set_id(*array++);
          size--;
          tofeentry.set_slatid(*array++);
          size--;
          for (int i = 0;i < 3;i++)
            {
              tofeentry.set_xyz(i, useInt::GetFloat(*array++));
              size--;
            }
          tofeentry.set_etof(useInt::GetFloat(*array++));
          size--;
          tofeentry.set_ttof(useInt::GetFloat(*array++));
          size--;

          tofemap->AddHit(tofeentry.get_id(), tofeentry);
        }
#ifdef DUMP
      tofemap->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverTofeHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
