#include <RecoverPadHits.h>
#include "setIntflag.h"

#include <PadHitMapEntry.h>
#include <PadHitMap.h>
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

//#ifdef DUMP
//ofstream dumprecover[3];
//#endif

RecoverPadHits::RecoverPadHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumprecover[0].open("/phenix/scratch/frawley/recoverpc1hits.dump");
  dumprecover[1].open("/phenix/scratch/frawley/recoverpc2hits.dump");
  dumprecover[2].open("/phenix/scratch/frawley/recoverpc3hits.dump");
#endif

  return;
}

int
RecoverPadHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  ostringstream tmpstream;
#ifdef useInt
  VariableArrayInt *padhit;
#else
  VariableArray *padhit;
#endif

  PadHitMap *padmap;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "Pc" << j << "Hit_VarArray";
      string NodeName = tmpstream.str();
#ifdef useInt
      padhit = findNode::getClass<VariableArrayInt>(topNode, NodeName.c_str());
#else
      padhit = findNode::getClass<VariableArray>(topNode, NodeName);
#endif
      if (padhit)
        {
          tmpstream.str(""); // reset tmpstream
          tmpstream << "Pc" << j << "Hit_comp";
          padmap  = findNode::getClass<PadHitMap>(topNode, tmpstream.str());
          if (!padmap)
            {
              padmap = new PadHitMap();
              PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(padmap,tmpstream.str().c_str() , "PHObject");
              dstNode->addNode(PHObjectNode);
            }
        }
      //      padhit->identify();
    }
  return EVENT_OK;
}

int
RecoverPadHits::process_event(PHCompositeNode *topNode)
{
  ostringstream PhobjectNodeName;
  for (short int j = 1; j <= 3; j++)
    {
      PhobjectNodeName.str("");
      PhobjectNodeName << "Pc" << j << "Hit_VarArray";
#ifdef useInt
      VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, PhobjectNodeName.str().c_str());
#else
      VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, PhobjectNodeName.str());
#endif
      if (hitarray)
        {
          PhobjectNodeName.str("");
          PhobjectNodeName << "Pc" << j << "Hit_comp";
          PadHitMap *padmap  = findNode::getClass<PadHitMap>(topNode, PhobjectNodeName.str());
          if (!padmap)
            {
              cout << PHWHERE << "Fatal: Cannot locate " <<  PhobjectNodeName.str() << endl;
              exit(1);
            }
          unsigned int size = hitarray->get_array_size();
#ifdef useInt
          const int *array = hitarray->get_array();
#else
          const short int *array = hitarray->get_array();
#endif
          PadHitMapEntry padentry;
          while (size > 0)
            {
              padentry.set_id(*array++);
              size--;
              for (int i = 0;i < 3;i++)
                {
                  //fi.i32 = *array++;
                  padentry.set_xyz(i, useInt::GetFloat(*array++));
                  size--;
                }
	      padmap->AddHit(padentry.get_id(),padentry);
            }
#ifdef DUMP
	  padmap->identify(dumprecover[j-1]);
#endif
        }
    }
  return EVENT_OK;
}

int
RecoverPadHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  for (int i=0; i<3;i++)
    {
      dumprecover[i].close();
    }
#endif

  return 0;
}
