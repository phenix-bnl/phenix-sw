//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcGeaTrackClusterWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcGeaTrackCluster.h"

using namespace std;

typedef PHIODataNode<dEmcGeaTrackClusterWrapper> MyNode_t;

DumpdEmcGeaTrackCluster::DumpdEmcGeaTrackCluster(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcGeaTrackCluster::process_Node(PHNode *myNode)
{
  dEmcGeaTrackClusterWrapper *demcgeatrackcluster = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demcgeatrackcluster = thisNode->getData();
    }
  if (demcgeatrackcluster)
    {
      *fout << "RowCount(): " << demcgeatrackcluster->RowCount() << endl;
      for (unsigned int i = 0; i < demcgeatrackcluster->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << demcgeatrackcluster->get_id(i) << endl;
	  *fout << "get_trkno(" << i << "): " << demcgeatrackcluster->get_trkno(i) << endl;
	  *fout << "get_track_ptr(" << i << "): " << demcgeatrackcluster->get_track_ptr(i) << endl;
	  *fout << "get_input(" << i << "): " << demcgeatrackcluster->get_input(i) << endl;
	  *fout << "get_pid(" << i << "): " << demcgeatrackcluster->get_pid(i) << endl;
	  *fout << "get_ptot(" << i << "): " << demcgeatrackcluster->get_ptot(i) << endl;
	  *fout << "get_nom_edep(" << i << "): " << demcgeatrackcluster->get_nom_edep(i) << endl;

	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_clusid(" << j << "," << i << "): " << demcgeatrackcluster->get_clusid(j,i) << endl;
	      *fout << "get_edep(" << j << "," << i << "): " << demcgeatrackcluster->get_edep(j,i) << endl;
	      *fout << "get_efrac(" << j << "," << i << "): " << demcgeatrackcluster->get_efrac(j,i) << endl;
	    }
        }
    }
  return 0;
}

