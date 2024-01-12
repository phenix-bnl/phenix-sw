#include <DumpHbdCellList.h>

#include <HbdCellList.h>
#include <HbdCell.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<HbdCellList> MyNode_t;

DumpHbdCellList::DumpHbdCellList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpHbdCellList::process_Node(PHNode *myNode)
{
  HbdCellList *hbdcelllist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      hbdcelllist = thisNode->getData();
    }
  if (hbdcelllist && hbdcelllist->isValid())
    {
      *fout << "hbdcelllist->get_nCells(): " << hbdcelllist->get_nCells() << endl;
      for (int i = 0; i < hbdcelllist->get_nCells(); i++)
        {
	  HbdCell *cell = hbdcelllist->get_cell(i);
          *fout << "cell->get_padnum(): " << cell->get_padnum() << endl;
          *fout << "cell->get_sector(): " << cell->get_sector() << endl;
          *fout << "cell->get_charge(): " << cell->get_charge() << endl;
          *fout << "cell->get_clusterid(): " << cell->get_clusterid() << endl;
        }
    }
  return 0;
}

