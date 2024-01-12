//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "tecghitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumptecghit.h"

using namespace std;

typedef PHIODataNode<tecghitWrapper> MyNode_t;

Dumptecghit::Dumptecghit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumptecghit::process_Node(PHNode *myNode)
{
  tecghitWrapper *tecghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tecghit = thisNode->getData();
    }
  if (tecghit && tecghit->RowCount())
    {
      *fout << "tecghit->RowCount(): " << tecghit->RowCount() << endl;
      for (unsigned int i = 0; i < tecghit->RowCount(); i++)
        {
          *fout << "tecghit->get_tof(" << i << "): " << tecghit->get_tof(i) << endl;
          *fout << "tecghit->get_dedx(" << i << "): " << tecghit->get_dedx(i) << endl;
          *fout << "tecghit->get_id(" << i << "): " << tecghit->get_id(i) << endl;
          *fout << "tecghit->get_arm(" << i << "): " << tecghit->get_arm(i) << endl;
          *fout << "tecghit->get_plane(" << i << "): " << tecghit->get_plane(i) << endl;
          *fout << "tecghit->get_sector(" << i << "): " << tecghit->get_sector(i) << endl;
          *fout << "tecghit->get_mctrack(" << i << "): " << tecghit->get_mctrack(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "tecghit->xyzinloc(" << j << "," << i << "): " << tecghit->get_xyzinloc(j, i) << endl;
              *fout << "tecghit->xyzoutloc(" << j << "," << i << "): " << tecghit->get_xyzoutloc(j, i) << endl;
              *fout << "tecghit->xyzinglo(" << j << "," << i << "): " << tecghit->get_xyzinglo(j, i) << endl;
            }
        }
    }
  return 0;
}

