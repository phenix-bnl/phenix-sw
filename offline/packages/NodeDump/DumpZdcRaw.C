#include <DumpZdcRaw.h>
#include <ZdcRaw.h>
#include <PHIODataNode.h>
#include <string>

using namespace std;

typedef PHIODataNode<ZdcRaw> MyNode_t;

DumpZdcRaw::DumpZdcRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int 
DumpZdcRaw::process_Node(PHNode *myNode)
{
  ZdcRaw *zdcraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      zdcraw = thisNode->getData();
    }
  if (zdcraw)
    {
      *fout << "ZdcRaw->isValid(): " << zdcraw->isValid() << endl;
      for (short int i = 0; i < 8; i++)
        {
              *fout << "get_Adc(" << i << "): " << zdcraw->get_Adc(i) << endl;
              *fout << "get_Tdc0(" << i << "): " << zdcraw->get_Tdc0(i) << endl;
              *fout << "get_Tdc1(" << i << "): " << zdcraw->get_Tdc1(i) << endl;
        }
    }
  return 0;
}
