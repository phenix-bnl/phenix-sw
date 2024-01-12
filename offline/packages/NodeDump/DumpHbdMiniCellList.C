#include <DumpHbdMiniCellList.h>

#include <HbdMiniCellList.h>
#include <HbdMiniCell.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<HbdMiniCellList> MyNode_t;

DumpHbdMiniCellList::DumpHbdMiniCellList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpHbdMiniCellList::process_Node(PHNode *myNode)
{
  HbdMiniCellList *hbdminicelllist = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      hbdminicelllist = thisNode->getData();
    }
  if (hbdminicelllist && hbdminicelllist->isValid())
    {
      *fout << "hbdminicelllist->get_nCells(): " << hbdminicelllist->get_nCells() << endl;
      for (int i = 0; i < hbdminicelllist->get_nCells(); i++)
        {
          HbdMiniCell *hbdminicell = hbdminicelllist->get_cell(i);
          *fout << "hbdminicell->get_stripid(): " << hbdminicell->get_adcch() << endl;
          *fout << "hbdminicell->get_chamberid(): " << hbdminicell->get_charge() << endl;
        }
    }
  return 0;
}
