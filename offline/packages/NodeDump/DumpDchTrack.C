#include "DumpDchTrack.h"


#include <DchTrack.h>
#include <PHPoint.h>

#include <PHIODataNode.h>

#include <cmath>
#include <string>

using namespace std;

typedef PHIODataNode<DchTrack> MyNode_t;

DumpDchTrack::DumpDchTrack(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpDchTrack::process_Node(PHNode *myNode)
{
  DchTrack *dchtrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dchtrack = thisNode->getData();
    }
  if (dchtrack && dchtrack->isValid())
    {
// uncomment this to write only events with those alpha = 0 tracks
//       int dowrite = 0;
//       for (unsigned int i = 0; i < dchtrack->get_DchNTrack(); i++)
//         {
// 	  if (dchtrack->get_alpha(i) == 0)
// 	    {
// 	      dowrite = 1;
// 	    }
// 	}
//       if (!dowrite)
// 	{
// 	  return 0;
// 	}
      *fout << "DchTrack dchtrack->get_DchNTrack() " << dchtrack->get_DchNTrack() << endl;
      for (unsigned int i = 0; i < dchtrack->get_DchNTrack(); i++)
	{
	  *fout << "Track No: " << i << endl;
	  *fout << "get_arm(" << i << "): " << dchtrack->get_arm(i) << endl;
	  for (int j = 0; j < 40; j++)
	    {
	      *fout << "get_hits(" << i << "," << j << "): " << dchtrack->get_hits(i, j) << endl;
	    }
	  *fout << "get_nx1hits(" << i << "): " << dchtrack->get_nx1hits(i) << endl;
	  *fout << "get_nx2hits(" << i << "): " << dchtrack->get_nx2hits(i) << endl;
	  *fout << "get_pc1hit(" << i << "): " << dchtrack->get_pc1hit(i) << endl;
	  *fout << "get_quality(" << i << "): " << dchtrack->get_quality(i) << endl;
	  *fout << "get_side(" << i << "): " << dchtrack->get_side(i) << endl;
	  *fout << "get_trackid(" << i << "): " << dchtrack->get_trackid(i) << endl;
	  *fout << "get_status(" << i << "): " << dchtrack->get_status(i) << endl;
	  *fout << "get_alpha(" << i << "): " << dchtrack->get_alpha(i) << endl;
	  *fout << "get_alpha1(" << i << "): " << dchtrack->get_alpha1(i) << endl;
	  *fout << "get_alpha2(" << i << "): " << dchtrack->get_alpha2(i) << endl;
	  *fout << "get_beta(" << i << "): " << dchtrack->get_beta(i) << endl;
	  *fout << "get_betaNoVertex(" << i << "): " << dchtrack->get_betaNoVertex(i) << endl;
	  *fout << "get_chi21(" << i << "): " << dchtrack->get_chi21(i) << endl;
	  *fout << "get_chi22(" << i << "): " << dchtrack->get_chi22(i) << endl;
	  *fout << "get_dist1(" << i << "): " << dchtrack->get_dist1(i) << endl;
	  *fout << "get_dist2(" << i << "): " << dchtrack->get_dist2(i) << endl;
	  *fout << "get_momentum(" << i << "): " << dchtrack->get_momentum(i) << endl;
	  *fout << "get_phi(" << i << "): " << dchtrack->get_phi(i) << endl;
	  if (finite(dchtrack->get_phi0(i)))
	    {
	      *fout << "get_phi0(" << i << "): " << dchtrack->get_phi0(i) << endl;
	    }
	  else
	    {
	      cout << "Dch: Bad phi0: " << dchtrack->get_phi0(i)
		   << " for track " << i
		   << " momentum: " << dchtrack->get_momentum(i)
		   << endl;
	    }
	  *fout << "get_theta0(" << i << "): " << dchtrack->get_theta0(i) << endl;
	  *fout << "get_zed(" << i << "): " << dchtrack->get_zed(i) << endl;
	  PHPoint point = dchtrack->get_point(i);
	  *fout << "get_point(" << i << ") x: " << point.getX()
		<< ", y: " << point.getY()
		<< ", z: " << point.getZ() << endl;
	  // 	  point = dchtrack->get_err_point(i);
	  // 	  *fout << "get_err_point(" << i << ") x: " << point.getX()
	  // 		<< ", y: " << point.getY()
	  // 		<< ", z: " << point.getZ() << endl;
	  point = dchtrack->get_direction(i);
	  *fout << "get_direction(" << i << ") x: " << point.getX()
		<< ", y: " << point.getY()
		<< ", z: " << point.getZ() << endl;
	  // 	  point = dchtrack->get_err_direction(i);
	  // 	  *fout << "get_err_direction(" << i << ") x: " << point.getX()
	  // 		<< ", y: " << point.getY()
// 		<< ", z: " << point.getZ() << endl;
        }
    }
  return 0;
}

