//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dcghitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumpdcghit.h"

using namespace std;

typedef PHIODataNode<dcghitWrapper> MyNode_t;

Dumpdcghit::Dumpdcghit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumpdcghit::process_Node(PHNode *myNode)
{
  dcghitWrapper *dcghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dcghit = thisNode->getData();
    }
  if (dcghit && dcghit->RowCount())
    {
      *fout << "RowCount(): " << dcghit->RowCount() << endl;
      for (unsigned int i = 0; i < dcghit->RowCount(); i++)
        {
          *fout << "dcghit->get_tof(" << i << "): " << dcghit->get_tof(i) << endl;
          *fout << "dcghit->get_pathLength(" << i << "): " << dcghit->get_pathLength(i) << endl;
          *fout << "dcghit->get_id(" << i << "): " << dcghit->get_id(i) << endl;
          *fout << "dcghit->get_plane(" << i << "): " << dcghit->get_plane(i) << endl;
          *fout << "dcghit->get_cell(" << i << "): " << dcghit->get_cell(i) << endl;
          *fout << "dcghit->get_arm(" << i << "): " << dcghit->get_arm(i) << endl;
          *fout << "dcghit->get_mctrack(" << i << "): " << dcghit->get_mctrack(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "dcghit->get_xyzinloc(" << i << "," << j << "): " << dcghit->get_xyzinloc(j, i) << endl;
              *fout << "dcghit->get_xyzoutloc(" << i << "," << j << "): " << dcghit->get_xyzoutloc(j, i) << endl;
              *fout << "dcghit->get_xyzinglo(" << i << "," << j << "): " << dcghit->get_xyzinglo(j, i) << endl;
            }
        }
    }
  return 0;
}

