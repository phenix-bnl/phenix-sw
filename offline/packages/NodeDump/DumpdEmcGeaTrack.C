//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcGeaTrackWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcGeaTrack.h"

using namespace std;

typedef PHIODataNode<dEmcGeaTrackWrapper> MyNode_t;

DumpdEmcGeaTrack::DumpdEmcGeaTrack(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcGeaTrack::process_Node(PHNode *myNode)
{
  dEmcGeaTrackWrapper *demcgea = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demcgea = thisNode->getData();
    }
  if (demcgea)
    {
      *fout << "RowCount(): " << demcgea->RowCount() << endl;
      for (unsigned int i = 0; i < demcgea->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << demcgea->get_id(i) << endl;
	  *fout << "get_trkno(" << i << "): " << demcgea->get_trkno(i) << endl;
	  *fout << "get_input(" << i << "): " << demcgea->get_input(i) << endl;
	  *fout << "get_anclvl(" << i << "): " << demcgea->get_anclvl(i) << endl;
	  *fout << "get_pid(" << i << "): " << demcgea->get_pid(i) << endl;
	  *fout << "get_ekin(" << i << "): " << demcgea->get_ekin(i) << endl;
	  *fout << "get_ptot(" << i << "): " << demcgea->get_ptot(i) << endl;
	  *fout << "get_itparent(" << i << "): " << demcgea->get_itparent(i) << endl;
	  *fout << "get_idparent(" << i << "): " << demcgea->get_idparent(i) << endl;
	  *fout << "get_parent_ptr(" << i << "): " << demcgea->get_parent_ptr(i) << endl;
	  *fout << "get_twrhit(" << i << "): " << demcgea->get_twrhit(i) << endl;
	  *fout << "get_edep(" << i << "): " << demcgea->get_edep(i) << endl;
	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_xyz(" << j << "," << i << "): " << demcgea->get_xyz(j,i) << endl;
	      *fout << "get_pxyz(" << j << "," << i << "): " << demcgea->get_pxyz(j,i) << endl;
	      *fout << "get_impxyz(" << j << "," << i << "): " << demcgea->get_impxyz(j,i) << endl;
	    }
        }
    }
  return 0;
}

