#include <string>

#include "PHIODataNode.h"

#include "lpcRaw.h"

#include "DumplpcRaw.h"

using namespace std;

typedef PHIODataNode<lpcRaw> MyNode_t;

DumplpcRaw::DumplpcRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumplpcRaw::process_Node(PHNode *myNode)
{
  lpcRaw *lpcraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      lpcraw = thisNode->getData();
    }
  if (lpcraw && lpcraw->isValid())
    {
      *fout << "get_npmt(): " << lpcraw->get_npmt() << endl;
      for (int i = 0; i < lpcraw->get_npmt(); i++)
        {
          *fout << "lpcRaw->get_AdcPost(" << i << "): " << lpcraw->get_AdcPost(i) << endl;
          *fout << "lpcRaw->get_AdcPre(" << i << "): " << lpcraw->get_AdcPre(i) << endl;
          *fout << "lpcRaw->get_Tdc0(" << i << "): " << lpcraw->get_Tdc0(i) << endl;
        }
    }
  return 0;
}

