#include <RecoverTofwHits.h>
#include "setIntflag.h"

#include <TofwHitMapEntry.h>
#include <TofwHitMap.h>
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

RecoverTofwHits::RecoverTofwHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recovertofwhits.dump");
#endif

  return;
}

int
RecoverTofwHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

#ifdef useIntflag
  VariableArrayInt *tofwhit = findNode::getClass<VariableArrayInt>(topNode, "TofwHit_VarArray");
#else
  VariableArray *tofwhit = findNode::getClass<VariableArray>(topNode, "TofwHit_VarArray");
#endif

  TofwHitMap *tofwmap;
  if (tofwhit)
    {
      tofwmap  = findNode::getClass<TofwHitMap>(topNode, "TofwHit_comp");
      if (!tofwmap)
        {
          tofwmap = new TofwHitMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(tofwmap, "TofwHit_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  //      tofwhit->identify();
  return EVENT_OK;
}

int
RecoverTofwHits::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "TofwHit_VarArray");
  const int *array;
  if(hitarray)
    array = hitarray->get_array();
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "TofwHit_VarArray");
  const short int *array;
  if(hitarray)
    array = hitarray->get_array();
#endif

  if (hitarray)
    {
      TofwHitMap *tofwmap  = findNode::getClass<TofwHitMap>(topNode, "TofwHit_comp");
      if (!tofwmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate TofwHit" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();

      TofwHitMapEntry tofwentry;
      while (size > 0)
        {
          tofwentry.set_id(*array++);
          size--;
          tofwentry.set_stripid(*array++);
          size--;
          for (int i = 0;i < 3;i++)
            {
              tofwentry.set_xyz(i, useInt::GetFloat(*array++));
              size--;
            }
          tofwentry.set_ttofw(useInt::GetFloat(*array++));
          size--;
          tofwentry.set_qtofw(useInt::GetFloat(*array++));
          size--;
          for (int i = 0; i < 2 ; i++)
            {
              tofwentry.set_adc(i, useInt::GetFloat(*array++));
              size--;
            }
          for (int i = 0; i < 2 ; i++)
            {
              tofwentry.set_tdc(i, useInt::GetFloat(*array++));
              size--;
            }
          tofwmap->AddHit(tofwentry.get_id(), tofwentry);
        }
#ifdef DUMP
      tofwmap->identify(dumprecover);
#endif
    }
  return EVENT_OK;
}

int
RecoverTofwHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}
