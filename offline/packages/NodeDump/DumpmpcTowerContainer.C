#include <string>

#include "PHIODataNode.h"

#include "mpcTowerContainer.h"
#include "mpcTowerContent.h"

#include "DumpmpcTowerContainer.h"

using namespace std;

typedef PHIODataNode<mpcTowerContainer> MyNode_t;

DumpmpcTowerContainer::DumpmpcTowerContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpmpcTowerContainer::process_Node(PHNode *myNode)
{
  mpcTowerContainer *mpctowercontainer = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mpctowercontainer = thisNode->getData();
    }
  if (mpctowercontainer && mpctowercontainer->isValid())
    {
      *fout << "Towers: " << mpctowercontainer->size() << endl;
      *fout << "mpctowercontainer->get_tdc_amu(): " << mpctowercontainer->get_tdc_amu() << endl;
      *fout << "mpctowercontainer->get_pre_amu(): " << mpctowercontainer->get_pre_amu() << endl;
      *fout << "mpctowercontainer->get_post_amu(): " << mpctowercontainer->get_post_amu() << endl;
      *fout << "mpctowercontainer->get_esum(): " << mpctowercontainer->get_esum() << endl;

      for (unsigned int i = 0; i < mpctowercontainer->size(); i++)
        {
          mpcTowerContent *thistower = mpctowercontainer->getTower(i);
          *fout << "get_ch(" << i << "): " << thistower->get_ch() << endl;
          *fout << "get_tof(" << i << "): " << thistower->get_tof() << endl;
          *fout << "get_energy(" << i << "): " << thistower->get_energy() << endl;
        }
    }
  return 0;
}

