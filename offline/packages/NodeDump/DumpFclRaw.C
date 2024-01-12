#include "DumpFclRaw.h"
#include "FclRaw.h"
#include "FclConsts.h"

#include "PHIODataNode.h" 
#include "PHNode.h"

#include <string>

using namespace std;

typedef PHIODataNode<FclRaw> MyNode_t;

DumpFclRaw::DumpFclRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpFclRaw::process_Node(PHNode *myNode)
{
  FclRaw *fclraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      fclraw = thisNode->getData();
    }
  if (fclraw && fclraw->isValid())
    {
      *fout << "getSide(): " << fclraw->getSide() << endl;
      *fout << "getCalibration(): " << fclraw->getCalibration() << endl;
      for (int i = 0; i < CHANTOT; i++)
        {
          *fout << "getHighAdcPost(" << i << "): " << fclraw->getHighAdcPost(i) << endl;
          *fout << "getHighAdcPre(" << i << "): " << fclraw->getHighAdcPre(i) << endl;
          *fout << "getHighGain(" << i << "): " << fclraw->getHighGain(i) << endl;
          *fout << "getHighGainCalib(" << i << "): " << fclraw->getHighGainCalib(i) << endl;
          *fout << "getLowGainCalib(" << i << "): " << fclraw->getLowGainCalib(i) << endl;
          *fout << "getTdc(" << i << "): " << fclraw->getTdc(i) << endl;
          *fout << "getLowAdcPost(" << i << "): " << fclraw->getLowAdcPost(i) << endl;
          *fout << "getLowAdcPre(" << i << "): " << fclraw->getLowAdcPre(i) << endl;
          *fout << "getLowGain(" << i << "): " << fclraw->getLowGain(i) << endl;

        }
    }
  return 0;
}

