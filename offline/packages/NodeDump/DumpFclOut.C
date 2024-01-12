#include "DumpFclOut.h"
#include "FclOut.h"
#include "FclConsts.h"

#include "PHIODataNode.h"
#include "PHNode.h"

#include <string>

using namespace std;

typedef PHIODataNode<FclOut> MyNode_t;

DumpFclOut::DumpFclOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpFclOut::process_Node(PHNode *myNode)
{
  FclOut *fclout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      fclout = thisNode->getData();
    }
  if (fclout && fclout->isValid())
    {
      *fout << "fclout->isValid(): " << fclout->isValid() << endl;
      *fout << "getSide(): " << fclout->getSide() << endl;
      *fout << "getSumAll(): " << fclout->getSumAll() << endl;
      *fout << "getSumGrey(): " << fclout->getSumGrey() << endl;
      for (int i = 0; i < CHANTOT; i++)
        {
          *fout << "getLowGain(" << i << "): " << fclout->getLowGain(i) << endl;        }
    }
  return 0;
}

