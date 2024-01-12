//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofReconstructedWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofReconstructed.h"

using namespace std;

typedef PHIODataNode<dTofReconstructedWrapper> MyNode_t;

DumpdTofReconstructed::DumpdTofReconstructed(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofReconstructed::process_Node(PHNode *myNode)
{
  dTofReconstructedWrapper *dtofout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofout = thisNode->getData();
    }
  if (dtofout && dtofout->RowCount())
    {
      *fout << "get_dTofNHit(): " << dtofout->RowCount() << endl;
      for (unsigned int i = 0; i < dtofout->RowCount(); i++)
        {
          *fout << "get_id(" << i << "): " << dtofout->get_id(i) << endl;
          *fout << "get_slatid(" << i << "): " << dtofout->get_slatid(i) << endl;
          *fout << "get_sector(" << i << "): " << dtofout->get_sector(i) << endl;
          *fout << "get_side(" << i << "): " << dtofout->get_side(i) << endl;
          *fout << "get_panel(" << i << "): " << dtofout->get_panel(i) << endl;
          *fout << "get_slat(" << i << "): " << dtofout->get_slat(i) << endl;
          *fout << "get_tof(" << i << "): " << dtofout->get_tof(i) << endl;
          *fout << "get_tof_err(" << i << "): " << dtofout->get_tof_err(i) << endl;
          *fout << "get_eloss(" << i << "): " << dtofout->get_eloss(i) << endl;
          *fout << "get_eloss_err(" << i << "): " << dtofout->get_eloss_err(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "get_xtof(" << i << "," << j << "): " << dtofout->get_xtof(j, i) << endl;
              *fout << "get_xtof_err(" << i << "," << j << "): " << dtofout->get_xtof_err(j, i) << endl;

            }
          for (short j = 0;j < 2;j++)
            {
              *fout << "get_qvc(" << i << "," << j << "): " << dtofout->get_qvc(j, i) << endl;
              *fout << "get_tvc(" << i << "," << j << "): " << dtofout->get_tvc(j, i) << endl;
            }
        }
    }
  return 0;
}

