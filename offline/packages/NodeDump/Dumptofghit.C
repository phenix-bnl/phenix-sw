//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "tofghitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumptofghit.h"

using namespace std;

typedef PHIODataNode<tofghitWrapper> MyNode_t;

Dumptofghit::Dumptofghit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumptofghit::process_Node(PHNode *myNode)
{
  tofghitWrapper *tofghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tofghit = thisNode->getData();
    }
  if (tofghit && tofghit->RowCount())
    {
      *fout << "get_tofNGhit(): " << tofghit->RowCount() << endl;
      for (unsigned int i = 0; i < tofghit->RowCount(); i++)
        {
          *fout << "tofghit->get_pos_hit_slat(" << i << "): " << tofghit->get_pos_hit_slat(i) << endl;
          *fout << "tofghit->get_tof(" << i << "): " << tofghit->get_tof(i) << endl;
          *fout << "tofghit->get_dele(" << i << "): " << tofghit->get_dele(i) << endl;
          *fout << "tofghit->get_subvol(" << i << "): " << tofghit->get_subvol(i) << endl;
          *fout << "tofghit->get_panel(" << i << "): " << tofghit->get_panel(i) << endl;
          *fout << "tofghit->get_column(" << i << "): " << tofghit->get_column(i) << endl;
          *fout << "tofghit->get_pslat(" << i << "): " << tofghit->get_pslat(i) << endl;
          *fout << "tofghit->get_slat_seq(" << i << "): " << tofghit->get_slat_seq(i) << endl;
          *fout << "tofghit->get_partl(" << i << "): " << tofghit->get_partl(i) << endl;
          *fout << "tofghit->get_mctrack(" << i << "): " << tofghit->get_mctrack(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "tofghit->get_pos_m(" << i << "," << j << "): " << tofghit->get_pos_m(j, i) << endl;
              *fout << "tofghit->get_p_m(" << i << "," << j << "): " << tofghit->get_p_m(j, i) << endl;
            }
        }
    }
  return 0;
}

