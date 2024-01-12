//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofFEMhitGhitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofFEMhitGhit.h"

using namespace std;

typedef PHIODataNode<dTofFEMhitGhitWrapper> MyNode_t;

DumpdTofFEMhitGhit::DumpdTofFEMhitGhit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofFEMhitGhit::process_Node(PHNode *myNode)
{
  dTofFEMhitGhitWrapper *dtoffemhitghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtoffemhitghit = thisNode->getData();
    }
  if (dtoffemhitghit && dtoffemhitghit->RowCount())
    {
      *fout << "RowCount(): " << dtoffemhitghit->RowCount() << endl;
      for (unsigned int i = 0; i < dtoffemhitghit->RowCount(); i++)
        {
          *fout << "toffemhitghit->get_crate(" << i << "): " << dtoffemhitghit->get_crate(i) << endl;
          *fout << "toffemhitghit->get_slot(" << i << "): " << dtoffemhitghit->get_slot(i) << endl;
          *fout << "toffemhitghit->get_slatid(" << i << "): " << dtoffemhitghit->get_slatid(i) << endl;
          *fout << "toffemhitghit->get_ghitid(" << i << "): " << dtoffemhitghit->get_ghitid(i) << endl;
	  for (int j = 0; j < 2 ; j++)
	    {
	      *fout << "toffemhitghit->get_ch(" << j << "," << i << "): " << dtoffemhitghit->get_ch(j,i) << endl;
	    }
        }
    }
  return 0;
}

