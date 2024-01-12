//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "AccHit.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpAccHit.h"

using namespace std;

typedef PHIODataNode<AccHit> MyNode_t;

DumpAccHit::DumpAccHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpAccHit::process_Node(PHNode *myNode)
{
  AccHit *acchit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      acchit = thisNode->getData();
    }
  if (acchit && acchit->isValid())
    {
      *fout << "AccHit->get_nhit: " << acchit->get_nhit() << endl;
      for (short int i = 0; i < acchit->get_nhit(); i++)
        {
          *fout << "AccHit->get_boxid(" << i << "): " << acchit->get_boxid(i) << endl;
          *fout << "AccHit->get_npe(" << i << "): " << acchit->get_npe(i) << endl;
          *fout << "AccHit->get_tof(" << i << "): " << acchit->get_tof(i) << endl;
          *fout << "AccHit->get_tdiff(" << i << "): " << acchit->get_tdiff(i) << endl;
          for (short int j = 0;j < 3;j++)
            {
              *fout << "AccHit->get_xyz(" << i << "," << j << "): " << acchit->get_xyz(i, j) << endl;
            }

        }

    }
  return 0;
}
