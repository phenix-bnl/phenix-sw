//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofRaw.h"

using namespace std;

typedef PHIODataNode<dTofRawWrapper> MyNode_t;

DumpdTofRaw::DumpdTofRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofRaw::process_Node(PHNode *myNode)
{
  dTofRawWrapper *dtofraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofraw = thisNode->getData();
    }
  if (dtofraw && dtofraw->RowCount())
    {
      *fout << "RowCount(): " << dtofraw->RowCount() << endl;
      for (unsigned int i = 0; i < dtofraw->RowCount(); i++)
        {
          *fout << "tofraw->get_id(" << i << "): " << dtofraw->get_id(i) << endl;
          *fout << "tofraw->get_slatid(" << i << "): " << dtofraw->get_slatid(i) << endl;
          *fout << "tofraw->get_sector(" << i << "): " << dtofraw->get_sector(i) << endl;
          *fout << "tofraw->get_side(" << i << "): " << dtofraw->get_side(i) << endl;
          *fout << "tofraw->get_panel(" << i << "): " << dtofraw->get_panel(i) << endl;
          *fout << "tofraw->get_slat(" << i << "): " << dtofraw->get_slat(i) << endl;
	  for(int j = 0; j < 2; j++)
	    {
	      *fout << "tofraw->get_cell(" << j << "," << i << "): " << dtofraw->get_cell(j,i) << endl;
	      *fout << "tofraw->get_qvc(" << j << "," << i << "): " << dtofraw->get_qvc(j,i) << endl;
	      *fout << "tofraw->get_q1(" << j << "," << i << "): " << dtofraw->get_q1(j,i) << endl;
	      *fout << "tofraw->get_q2(" << j << "," << i << "): " << dtofraw->get_q2(j,i) << endl;
	      *fout << "tofraw->get_tvc(" << j << "," << i << "): " << dtofraw->get_tvc(j,i) << endl;
	      *fout << "tofraw->get_t3(" << j << "," << i << "): " << dtofraw->get_t3(j,i) << endl;
	      *fout << "tofraw->get_t4(" << j << "," << i << "): " << dtofraw->get_t4(j,i) << endl;
	    }
        }
    }
  return 0;
}

