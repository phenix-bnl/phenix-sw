//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcGeaTrackTowerWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcGeaTrackTower.h"

using namespace std;

typedef PHIODataNode<dEmcGeaTrackTowerWrapper> MyNode_t;

DumpdEmcGeaTrackTower::DumpdEmcGeaTrackTower(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcGeaTrackTower::process_Node(PHNode *myNode)
{
  dEmcGeaTrackTowerWrapper *demcgeatracktower = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demcgeatracktower = thisNode->getData();
    }
  if (demcgeatracktower)
    {
      *fout << "RowCount(): " << demcgeatracktower->RowCount() << endl;
      for (unsigned int i = 0; i < demcgeatracktower->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << demcgeatracktower->get_id(i) << endl;
	  *fout << "get_trkno(" << i << "): " << demcgeatracktower->get_trkno(i) << endl;
	  *fout << "get_input(" << i << "): " << demcgeatracktower->get_input(i) << endl;
	  *fout << "get_nextid(" << i << "): " << demcgeatracktower->get_nextid(i) << endl;

	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_xyz(" << j << "," << i << "): " << demcgeatracktower->get_xyz(j,i) << endl;
	      *fout << "get_twrkey(" << j << "," << i << "): " << demcgeatracktower->get_twrkey(j,i) << endl;
	      *fout << "get_edep(" << j << "," << i << "): " << demcgeatracktower->get_edep(j,i) << endl;
	    }
        }
    }
  return 0;
}

