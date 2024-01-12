//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcGeaTowerTrackWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcGeaTowerTrack.h"

using namespace std;

typedef PHIODataNode<dEmcGeaTowerTrackWrapper> MyNode_t;

DumpdEmcGeaTowerTrack::DumpdEmcGeaTowerTrack(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcGeaTowerTrack::process_Node(PHNode *myNode)
{
  dEmcGeaTowerTrackWrapper *demcgeatowertrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demcgeatowertrack = thisNode->getData();
    }
  if (demcgeatowertrack)
    {
      *fout << "RowCount(): " << demcgeatowertrack->RowCount() << endl;
      for (unsigned int i = 0; i < demcgeatowertrack->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << demcgeatowertrack->get_id(i) << endl;
	  *fout << "get_twrkey(" << i << "): " << demcgeatowertrack->get_twrkey(i) << endl;
	  *fout << "get_input(" << i << "): " << demcgeatowertrack->get_input(i) << endl;

	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_trkno(" << j << "," << i << "): " << demcgeatowertrack->get_trkno(j,i) << endl;
	      *fout << "get_edep(" << j << "," << i << "): " << demcgeatowertrack->get_edep(j,i) << endl;
	      *fout << "get_toffirst(" << j << "," << i << "): " << demcgeatowertrack->get_toffirst(j,i) << endl;
	    }
        }
    }
  return 0;
}

