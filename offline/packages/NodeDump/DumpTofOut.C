//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "TofOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpTofOut.h"

using namespace std;

typedef PHIODataNode<TofOut> MyNode_t;

DumpTofOut::DumpTofOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpTofOut::process_Node(PHNode *myNode)
{
  TofOut *tofout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tofout = thisNode->getData();
    }
  if (tofout && tofout->isValid())
    {
      *fout << "get_TofNHit(): " << tofout->get_TofNHit() << endl;
      for (unsigned int i = 0; i < tofout->get_TofNHit(); i++)
        {
          *fout << "get_id(" << i << "): " << tofout->get_id(i) << endl;
          *fout << "get_slatid(" << i << "): " << tofout->get_slatid(i) << endl;
          *fout << "get_sector(" << i << "): " << tofout->get_sector(i) << endl;
          *fout << "get_side(" << i << "): " << tofout->get_side(i) << endl;
          *fout << "get_panel(" << i << "): " << tofout->get_panel(i) << endl;
          *fout << "get_slat(" << i << "): " << tofout->get_slat(i) << endl;
          *fout << "get_tof(" << i << "): " << tofout->get_tof(i) << endl;
          *fout << "get_tof_err(" << i << "): " << tofout->get_tof_err(i) << endl;
          *fout << "get_eloss(" << i << "): " << tofout->get_eloss(i) << endl;
          *fout << "get_eloss_err(" << i << "): " << tofout->get_eloss_err(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "get_xtof(" << i << "," << j << "): " << tofout->get_xtof(i, j) << endl;
              *fout << "get_xtof_err(" << i << "," << j << "): " << tofout->get_xtof_err(i, j) << endl;

            }
          for (short j = 0;j < 2;j++)
            {
              *fout << "get_qvc(" << i << "," << j << "): " << tofout->get_qvc(i, j) << endl;
              *fout << "get_tvc(" << i << "," << j << "): " << tofout->get_tvc(i, j) << endl;
            }
        }
    }
  return 0;
}

