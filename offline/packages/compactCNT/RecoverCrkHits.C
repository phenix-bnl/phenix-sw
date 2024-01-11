#include <RecoverCrkHits.h>
#include "setIntflag.h"

#include <CrkHitMapEntry.h>
#include <CrkHitMap.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>


#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <half/half.h>
#include <useInt.h>

#include <cstdlib>
#include <sstream>
#include <fstream>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

RecoverCrkHits::RecoverCrkHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recovercrkhits.dump");
#endif

  return;
}

int
RecoverCrkHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

#ifdef useIntflag
  VariableArrayInt *crkhit;
  crkhit = findNode::getClass<VariableArrayInt>(topNode, "CrkHit_VarArray");
#else
  VariableArray *crkhit;
  crkhit = findNode::getClass<VariableArray>(topNode, "CrkHit_VarArray");
#endif

  CrkHitMap *crkmap;
  if (crkhit)
    {
      crkmap  = findNode::getClass<CrkHitMap>(topNode, "CrkHit_comp");
      if (!crkmap)
        {
          crkmap = new CrkHitMap();
          PHIODataNode<PHObject> *PHObjectNode = new PHIODataNode<PHObject>(crkmap, "CrkHit_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }

  return EVENT_OK;
}

int
RecoverCrkHits::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
  VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "CrkHit_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "CrkHit_VarArray");
#endif

  if (hitarray)
    {
      CrkHitMap *crkmap  = findNode::getClass<CrkHitMap>(topNode, "CrkHit_comp");
      if (!crkmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate CrkHit" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif
      CrkHitMapEntry crkentry;
      while (size > 0)
        {
          crkentry.set_id(*array++);
          size--;
          crkentry.set_pmtid(*array++);
          size--;
          float npe = GetFloat(*array++); 
	    //fi.i32 = *array++;
          crkentry.set_npe(npe);
          size--;
          //fi.i32 = *array++;
	  float time = GetFloat(*array++);
          crkentry.set_time(time);
          size--;

          crkmap->AddHit(crkentry.get_id(), crkentry);
        }
#ifdef DUMP
      crkmap->identify(dumprecover);
#endif
    }

  return EVENT_OK;
}

int
RecoverCrkHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}


float
RecoverCrkHits::GetFloat(const int ival) const
{
  floatint fi;
  fi.i32 = ival;
  return fi.f32;
}

float
RecoverCrkHits::GetFloat(const short int ival) const
{
  half halfvar;
  halfvar.setBits(ival);
  return halfvar;
}

