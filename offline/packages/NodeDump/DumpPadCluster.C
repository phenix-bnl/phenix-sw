//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "PadCluster.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpPadCluster.h"

using namespace std;

typedef PHIODataNode<PadCluster> MyNode_t;

DumpPadCluster::DumpPadCluster(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPadCluster::process_Node(PHNode *myNode)
{
  PadCluster *padcluster = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      padcluster = thisNode->getData();
    }
  if (padcluster && padcluster->isValid())
    {
      *fout << "get_PadNCluster(): " << padcluster->get_PadNCluster() << endl;
      for (unsigned int i = 0; i < padcluster->get_PadNCluster(); i++)
        {
          *fout << "padcluster->get_id(" << i << "): " << padcluster->get_id(i) << endl;
          *fout << "padcluster->get_arm(" << i << "): " << padcluster->get_arm(i) << endl;
          *fout << "padcluster->get_sector(" << i << "): " << padcluster->get_sector(i) << endl;
          *fout << "padcluster->get_wire(" << i << "): " << padcluster->get_wire(i) << endl;
          *fout << "padcluster->get_cell(" << i << "): " << padcluster->get_cell(i) << endl;
          *fout << "padcluster->get_type(" << i << "): " << padcluster->get_type(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "padcluster->get_xyz(" << i << "," << j << "): " << padcluster->get_xyz(i, j) << endl;
              *fout << "padcluster->get_dxyz(" << i << "," << j << "): " << padcluster->get_dxyz(i, j) << endl;
            }
        }
    }
  return 0;
}

