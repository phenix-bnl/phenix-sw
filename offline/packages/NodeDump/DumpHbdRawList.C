#include <DumpHbdRawList.h>

#include <HbdRawList.h>
#include <HbdRaw.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<HbdRawList> MyNode_t;

DumpHbdRawList::DumpHbdRawList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpHbdRawList::process_Node(PHNode *myNode)
{
  HbdRawList *hbdrawlist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      hbdrawlist = thisNode->getData();
    }
  if (hbdrawlist && hbdrawlist->isValid())
    {
      *fout << "hbdrawlist->get_nRaws(): " << hbdrawlist->get_nRaws() << endl;
      for (int i = 0; i < hbdrawlist->get_nRaws(); i++)
        {
          HbdRaw *raw = hbdrawlist->get_raw(i);
          *fout << "raw->get_padid(): " << raw->get_padid() << endl;
          for (int j = 0;j < 32;j++)
            {
              *fout << "raw->get_clock(" << j << "): " << raw->get_clock(j) << endl;
              *fout << "raw->get_rawadc(" << j << "): " << raw->get_rawadc(j) << endl;
              *fout << "raw->get_charge(" << j << "): " << raw->get_charge(j) << endl;
            }
        }
    }
  return 0;
}
