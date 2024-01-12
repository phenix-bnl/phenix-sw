//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "pcghitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumppcghit.h"

using namespace std;

typedef PHIODataNode<pcghitWrapper> MyNode_t;

Dumppcghit::Dumppcghit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumppcghit::process_Node(PHNode *myNode)
{
  pcghitWrapper *pcghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      pcghit = thisNode->getData();
    }
  if (pcghit && pcghit->RowCount())
    {
      *fout << "get_pcNGhit(): " << pcghit->RowCount() << endl;
      for (unsigned int i = 0; i < pcghit->RowCount(); i++)
        {
          *fout << "pcghit->get_tof(" << i << "): " << pcghit->get_tof(i) << endl;
          *fout << "pcghit->get_dedx(" << i << "): " << pcghit->get_dedx(i) << endl;
          *fout << "pcghit->get_pathLength(" << i << "): " << pcghit->get_pathLength(i) << endl;
          *fout << "pcghit->get_id(" << i << "): " << pcghit->get_id(i) << endl;
          *fout << "pcghit->get_arm(" << i << "): " << pcghit->get_arm(i) << endl;
          *fout << "pcghit->get_sector(" << i << "): " << pcghit->get_sector(i) << endl;
          *fout << "pcghit->get_mctrack(" << i << "): " << pcghit->get_mctrack(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "pcghit->get_xyzinloc(" << i << "," << j << "): " << pcghit->get_xyzinloc(j, i) << endl;
              *fout << "pcghit->get_xyzoutloc(" << i << "," << j << "): " << pcghit->get_xyzoutloc(j, i) << endl;
              *fout << "pcghit->get_xyzinglo(" << i << "," << j << "): " << pcghit->get_xyzinglo(j, i) << endl;
            }
        }
    }
  return 0;
}

