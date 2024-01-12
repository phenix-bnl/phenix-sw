#include "DumpmpcRawContainer.h"

#include "mpcRawContainer.h"
#include "mpcRawContent.h"

#include "PHIODataNode.h"

#include <string>

using namespace std;

typedef PHIODataNode<mpcRawContainer> MyNode_t;

DumpmpcRawContainer::DumpmpcRawContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpmpcRawContainer::process_Node(PHNode *myNode)
{
  mpcRawContainer *mpcraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mpcraw = thisNode->getData();
    }
  if (mpcraw && mpcraw->isValid())
    {
      *fout << "mpcraw->get_tdc_amu(): " << mpcraw->get_tdc_amu() << endl;
      *fout << "mpcraw->get_pre_amu(): " << mpcraw->get_pre_amu() << endl;
      *fout << "mpcraw->get_post_amu(): " << mpcraw->get_post_amu() << endl;
      *fout << "mpcraw->size(): " << mpcraw->size() << endl;
      for (unsigned int i = 0; i < mpcraw->size(); i++)
        {
	  mpcRawContent *raw = mpcraw->getTower(i);
          *fout << "crystal " << i << ": mpcRawContent->get_ch(): " << raw->get_ch() << endl;
          *fout << "crystal " << i << ": mpcRawContent->get_tdc(): " << raw->get_tdc() << endl;
          *fout << "crystal " << i << ": mpcRawContent->get_lopost(): " << raw->get_lopost() << endl;
          *fout << "crystal " << i << ": mpcRawContent->get_lopre(): " << raw->get_lopre() << endl;
          *fout << "crystal " << i << ": mpcRawContent->get_hipost(): " << raw->get_hipost() << endl;
          *fout << "crystal " << i << ": mpcRawContent->get_hipre(): " << raw->get_hipre() << endl;
        }
    }
  return 0;
}

