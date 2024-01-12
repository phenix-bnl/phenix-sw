//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dPadClusterWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdPadCluster.h"

using namespace std;

typedef PHIODataNode<dPadClusterWrapper> MyNode_t;

DumpdPadCluster::DumpdPadCluster(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdPadCluster::process_Node(PHNode *myNode)
{
  dPadClusterWrapper *dpadcluster = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dpadcluster = thisNode->getData();
    }
  if (dpadcluster && dpadcluster->RowCount())
    {
      *fout << "get_dPadNCluster(): " << dpadcluster->RowCount() << endl;
      for (unsigned int i = 0; i < dpadcluster->RowCount(); i++)
        {
          *fout << "dpadcluster->get_id(" << i << "): " << dpadcluster->get_id(i) << endl;
          *fout << "dpadcluster->get_arm(" << i << "): " << dpadcluster->get_arm(i) << endl;
          *fout << "dpadcluster->get_sector(" << i << "): " << dpadcluster->get_sector(i) << endl;
          *fout << "dpadcluster->get_wire(" << i << "): " << dpadcluster->get_wire(i) << endl;
          *fout << "dpadcluster->get_cell(" << i << "): " << dpadcluster->get_cell(i) << endl;
          *fout << "dpadcluster->get_type(" << i << "): " << dpadcluster->get_type(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "dpadcluster->get_xyz(" << i << "," << j << "): " << dpadcluster->get_xyz(j, i) << endl;
              *fout << "dpadcluster->get_dxyz(" << i << "," << j << "): " << dpadcluster->get_dxyz(j, i) << endl;
            }
        }
    }
  return 0;
}

