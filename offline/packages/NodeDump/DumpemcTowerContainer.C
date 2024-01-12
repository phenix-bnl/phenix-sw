//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "emcTowerContainer.h"
#include "emcTowerContent.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpemcTowerContainer.h"

using namespace std;

typedef PHIODataNode<emcTowerContainer> MyNode_t;

DumpemcTowerContainer::DumpemcTowerContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpemcTowerContainer::process_Node(PHNode *myNode)
{
  emcTowerContainer *emctowercontainer = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      emctowercontainer = thisNode->getData();
    }
  if (emctowercontainer && emctowercontainer->isValid())
    {
      *fout << "Towers: " << emctowercontainer->size() << endl;
      for (unsigned int i = 0; i < emctowercontainer->size(); i++)
        {
          emcTowerContent *thistower = emctowercontainer->getTower(i);
	  thistower->ShutUp(); // kill these stupid virtual warnings
          *fout << "ADC(" << i << "): " << thistower->ADC() << endl;
          *fout << "AMUPre(" << i << "): " << thistower->AMUPre() << endl;
          *fout << "AMUPost(" << i << "): " << thistower->AMUPost() << endl;
          *fout << "AMUTAC(" << i << "): " << thistower->AMUTAC() << endl;
          *fout << "BeamClock(" << i << "): " << thistower->BeamClock() << endl;
          *fout << "Channel(" << i << "): " << thistower->Channel() << endl;
          *fout << "DataError(" << i << "): " << thistower->DataError() << endl;
          *fout << "Energy(" << i << "): " << thistower->Energy() << endl;
          *fout << "ErrorNeighbours(" << i << "): " << thistower->ErrorNeighbours() << endl;
          *fout << "FEM(" << i << "): " << thistower->FEM() << endl;
          *fout << "hasReference(" << i << "): " << thistower->hasReference() << endl;
          *fout << "hasCalib(" << i << "): " << thistower->hasCalib() << endl;
          *fout << "hasDC(" << i << "): " << thistower->hasDC() << endl;
          *fout << "hasRaw(" << i << "): " << thistower->hasRaw() << endl;
          *fout << "HG(" << i << "): " << thistower->HG() << endl;
          *fout << "HGPost(" << i << "): " << thistower->HGPost() << endl;
          *fout << "HGPre(" << i << "): " << thistower->HGPre() << endl;
          *fout << "HGPP(" << i << "): " << thistower->HGPP() << endl;
          *fout << "isSimulated(" << i << "): " << thistower->isSimulated() << endl;
          *fout << "isMerged(" << i << "): " << thistower->isMerged() << endl;
          *fout << "isReference(" << i << "): " << thistower->isReference() << endl;
          *fout << "isValid(" << i << "): " << thistower->isValid() << endl;
          *fout << "isZero(" << i << "): " << thistower->isZero() << endl;
          *fout << "LG(" << i << "): " << thistower->LG() << endl;
          *fout << "LGPost(" << i << "): " << thistower->LGPost() << endl;
          *fout << "LGPre(" << i << "): " << thistower->LGPre() << endl;
          *fout << "LGPP(" << i << "): " << thistower->LGPP() << endl;
          *fout << "TAC(" << i << "): " << thistower->TAC() << endl;
          *fout << "TDC(" << i << "): " << thistower->TDC() << endl;
          *fout << "ToF(" << i << "): " << thistower->ToF() << endl;
          *fout << "TowerID(" << i << "): " << thistower->TowerID() << endl;
          *fout << "WarnNeighbours(" << i << "): " << thistower->WarnNeighbours() << endl;
        }
    }
  return 0;
}

