//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dDchTracksWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdDchTracks.h"

using namespace std;

typedef PHIODataNode<dDchTracksWrapper> MyNode_t;

DumpdDchTracks::DumpdDchTracks(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdDchTracks::process_Node(PHNode *myNode)
{
  dDchTracksWrapper *ddchtracks = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      ddchtracks = thisNode->getData();
    }
  if (ddchtracks)
    {
      *fout << "RowCount(): " << ddchtracks->RowCount() << endl;
      for (unsigned int i = 0; i < ddchtracks->RowCount(); i++)
        {
	  *fout << "get_trackid(" << i << "): " << ddchtracks->get_trackid(i) << endl;
	  *fout << "get_arm(" << i << "): " << ddchtracks->get_arm(i) << endl;
	  *fout << "get_side(" << i << "): " << ddchtracks->get_side(i) << endl;
	  *fout << "get_quality(" << i << "): " << ddchtracks->get_quality(i) << endl;
	  *fout << "get_phi(" << i << "): " << ddchtracks->get_phi(i) << endl;
	  *fout << "get_alpha(" << i << "): " << ddchtracks->get_alpha(i) << endl;
	  *fout << "get_beta(" << i << "): " << ddchtracks->get_beta(i) << endl;
	  *fout << "get_betaNoVertex(" << i << "): " << ddchtracks->get_betaNoVertex(i) << endl;
	  *fout << "get_zed(" << i << "): " << ddchtracks->get_zed(i) << endl;
	  *fout << "get_phi0(" << i << "): " << ddchtracks->get_phi0(i) << endl;
	  *fout << "get_theta0(" << i << "): " << ddchtracks->get_theta0(i) << endl;
	  *fout << "get_momentum(" << i << "): " << ddchtracks->get_momentum(i) << endl;
	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_point(" << j << "," << i << "): " << ddchtracks->get_point(j,i) << endl;
	      *fout << "get_err_point(" << j << "," << i << "): " << ddchtracks->get_err_point(j,i) << endl;
	      *fout << "get_direction(" << j << "," << i << "): " << ddchtracks->get_direction(j,i) << endl;
	      *fout << "get_err_direction(" << j << "," << i << "): " << ddchtracks->get_err_direction(j,i) << endl;
	    }
	  for (short j = 0;j<40;j++)
	    {
	      *fout << "get_hits(" << j << "," << i << "): " << ddchtracks->get_hits(j,i) << endl;
	    }
        }
    }
  return 0;
}

