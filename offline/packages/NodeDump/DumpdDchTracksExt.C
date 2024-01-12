//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dDchTracksExtWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdDchTracksExt.h"

using namespace std;

typedef PHIODataNode<dDchTracksExtWrapper> MyNode_t;

DumpdDchTracksExt::DumpdDchTracksExt(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdDchTracksExt::process_Node(PHNode *myNode)
{
  dDchTracksExtWrapper *ddchtracksext = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      ddchtracksext = thisNode->getData();
    }
  if (ddchtracksext)
    {
      *fout << "RowCount(): " << ddchtracksext->RowCount() << endl;
      for (unsigned int i = 0; i < ddchtracksext->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << ddchtracksext->get_id(i) << endl;
	  *fout << "get_status(" << i << "): " << ddchtracksext->get_status(i) << endl;
	  *fout << "get_alpha1(" << i << "): " << ddchtracksext->get_alpha1(i) << endl;
	  *fout << "get_alpha2(" << i << "): " << ddchtracksext->get_alpha2(i) << endl;
	  *fout << "get_dist1(" << i << "): " << ddchtracksext->get_dist1(i) << endl;
	  *fout << "get_dist2(" << i << "): " << ddchtracksext->get_dist2(i) << endl;
	  *fout << "get_chi21(" << i << "): " << ddchtracksext->get_chi21(i) << endl;
	  *fout << "get_chi22(" << i << "): " << ddchtracksext->get_chi22(i) << endl;
	  *fout << "get_nx1hits(" << i << "): " << (short) ddchtracksext->get_nx1hits(i) << endl;
	  *fout << "get_nx2hits(" << i << "): " << (short) ddchtracksext->get_nx2hits(i) << endl;
        }
    }
  return 0;
}

