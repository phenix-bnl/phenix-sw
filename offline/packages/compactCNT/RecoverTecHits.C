#include <RecoverTecHits.h>
#include "setIntflag.h"

#include <TecHitMapEntry.h>
#include <TecHitMap.h>
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

RecoverTecHits::RecoverTecHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/recovertechits.dump");
#endif

  return;
}

int
RecoverTecHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

#ifdef useIntflag
  VariableArrayInt *techit;
  techit = findNode::getClass<VariableArrayInt>(topNode, "TecHit_VarArray");
#else
  VariableArray *techit;
  techit = findNode::getClass<VariableArray>(topNode, "TecHit_VarArray");
#endif

  TecHitMap *tecmap;
  if (techit)
    {
      tecmap  = findNode::getClass<TecHitMap>(topNode, "TecHit_comp");
      if (!tecmap)
        {
          tecmap = new TecHitMap();
          PHDataNode<PHObject> *PHObjectNode = new PHDataNode<PHObject>(tecmap, "TecHit_comp" , "PHObject");
          dstNode->addNode(PHObjectNode);
        }
    }
  //      techit->identify();
  return EVENT_OK;
}

int
RecoverTecHits::process_event(PHCompositeNode *topNode)
{

#ifdef useIntflag
    VariableArrayInt *hitarray = findNode::getClass<VariableArrayInt>(topNode, "TecHit_VarArray");
#else
  VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, "TecHit_VarArray");
#endif

  if (hitarray)
    {
      TecHitMap *tecmap  = findNode::getClass<TecHitMap>(topNode, "TecHit_comp");
      if (!tecmap)
        {
          cout << PHWHERE << "Fatal: Cannot locate TecHit_comp" << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();
#endif
      TecHitMapEntry tecentry;

#ifdef DUMP
      dumpfile << "Size of TecHit_VarArray is " << size << endl;
#endif 
      short int nentry=0;
 
     while (size > 0)
        {
          tecentry.set_id(*array++);   // Tec cluster number
          size--;
          tecentry.set_index(*array++);
          size--;
          tecentry.set_wire(*array++);
          size--;
	  tecentry.set_avgtime(*array++);
	  size--;
          tecentry.set_ntimebins(*array++);
          size--;
	  float charge = GetFloat(*array++);
          tecentry.set_charge(charge);
          size--;
	  
          tecmap->AddHit(nentry, tecentry);
	  nentry++;
        }
#ifdef DUMP
      tecmap->identify(dumpfile);
#endif
    }
  /*
  else
    {
      cout << "Did not find TecHit_Varrarray" << endl;
    }
  */

  return EVENT_OK;
}

int
RecoverTecHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

float
RecoverTecHits::GetFloat(const int ival) const
{
  floatint fi;
  fi.i32 = ival;
  return fi.f32;
}

float
RecoverTecHits::GetFloat(const short int ival) const
{
    half halfvar;
    halfvar.setBits(ival);
    return halfvar;
}
