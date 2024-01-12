//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dDchHitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdDchHit.h"

using namespace std;

typedef PHIODataNode<dDchHitWrapper> MyNode_t;

DumpdDchHit::DumpdDchHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdDchHit::process_Node(PHNode *myNode)
{
  dDchHitWrapper *ddchhit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      ddchhit = thisNode->getData();
    }
  if (ddchhit)
    {
      *fout << "RowCount(): " << ddchhit->RowCount() << endl;
      for (unsigned int i = 0; i < ddchhit->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << ddchhit->get_id(i) << endl;
	  *fout << "get_arm(" << i << "): " << ddchhit->get_arm(i) << endl;
	  *fout << "get_plane(" << i << "): " << ddchhit->get_plane(i) << endl;
	  *fout << "get_cell(" << i << "): " << ddchhit->get_cell(i) << endl;
	  *fout << "get_side(" << i << "): " << ddchhit->get_side(i) << endl;
	  *fout << "get_distance(" << i << "): " << ddchhit->get_distance(i) << endl;
	  *fout << "get_width(" << i << "): " << ddchhit->get_width(i) << endl;
	  *fout << "get_idraw1(" << i << "): " << ddchhit->get_idraw1(i) << endl;
	  *fout << "get_idraw2(" << i << "): " << ddchhit->get_idraw2(i) << endl;
	  *fout << "get_idmirror(" << i << "): " << ddchhit->get_idmirror(i) << endl;
	  *fout << "get_time1(" << i << "): " << ddchhit->get_time1(i) << endl;
	  *fout << "get_time2(" << i << "): " << ddchhit->get_time2(i) << endl;
	  *fout << "get_used(" << i << "): " << ddchhit->get_used(i) << endl;
	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_xyz(" << j << "," << i << "): " << ddchhit->get_xyz(j,i) << endl;
	      *fout << "get_err(" << j << "," << i << "): " << ddchhit->get_err(j,i) << endl;
	      *fout << "get_vxyz(" << j << "," << i << "): " << ddchhit->get_vxyz(j,i) << endl;
	    }
        }
    }
  return 0;
}

