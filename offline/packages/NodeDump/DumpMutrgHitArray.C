#include <DumpMutrgHitArray.h>

#include <MutrgHitArray.hh>
#include <MutrgHit.hh>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<MutrgHitArray> MyNode_t;

DumpMutrgHitArray::DumpMutrgHitArray(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMutrgHitArray::process_Node(PHNode *myNode)
{
  const MutrgHitArray::private_map *hitmap;
  MutrgHitArray::const_private_itr iter;
  int arm;
  int station;
  int octant;
  int halfoctant;
  int gap;
  int cathode;
  int strip;
  MutrgHitArray *mutrghitarray = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mutrghitarray = thisNode->getData();
    }
  //  if (mutrghitarray && mutrghitarray->isValid())
  if (mutrghitarray)
    {
      hitmap = mutrghitarray->Get();
      *fout << "mutrghits: " << hitmap->size() << endl;
      int i=0;
      for (iter = hitmap->begin(); iter != hitmap->end(); iter++)
	{
	  (iter->second)->GetLocation(arm,station,octant,halfoctant,gap,cathode,strip);
	  *fout << "arm(" << i << "): " << arm << endl;
	  *fout << "station(" << i << "): " << station << endl;
	  *fout << "octant(" << i << "): " << octant << endl;
	  *fout << "halfoctant(" << i << "): " << halfoctant << endl;
	  *fout << "gap(" << i << "): " << gap << endl;
	  *fout << "cathode(" << i << "): " << cathode << endl;
	  *fout << "strip(" << i << "): " << strip << endl;
	  *fout << "GetHitClock(" << i << "): " <<  (iter->second)->GetHitClock() << endl;
	  i++;
	}
    }
  return 0;
}
