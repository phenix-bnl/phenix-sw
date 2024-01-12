//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "primaryWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumpprimary.h"

using namespace std;

typedef PHIODataNode<primaryWrapper> MyNode_t;

Dumpprimary::Dumpprimary(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumpprimary::process_Node(PHNode *myNode)
{
  primaryWrapper *primary = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      primary = thisNode->getData();
    }
  if (primary)
    {
      *fout << "RowCount(): " << primary->RowCount() << endl;
      for (unsigned int i = 0; i < primary->RowCount(); i++)
        {
	  *fout << "get_key(" << i << "): " << primary->get_key(i) << endl;
	  *fout << "get_event_track(" << i << "): " << primary->get_event_track(i) << endl;
	  *fout << "get_subevent_track(" << i << "): " << primary->get_subevent_track(i) << endl;
	  *fout << "get_true_track(" << i << "): " << primary->get_true_track(i) << endl;
	  *fout << "get_subevent(" << i << "): " << primary->get_subevent(i) << endl;
	  *fout << "get_idpart(" << i << "): " << primary->get_idpart(i) << endl;
	  *fout << "get_nfile(" << i << "): " << primary->get_nfile(i) << endl;
	  *fout << "get_px_momentum(" << i << "): " << primary->get_px_momentum(i) << endl;
	  *fout << "get_py_momentum(" << i << "): " << primary->get_py_momentum(i) << endl;
	  *fout << "get_pz_momentum(" << i << "): " << primary->get_pz_momentum(i) << endl;
        }
    }
  return 0;
}

