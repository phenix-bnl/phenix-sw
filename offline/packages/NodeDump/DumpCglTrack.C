#include <string>

#include "PHIODataNode.h"

#include "CglTrack.h"

#include "DumpCglTrack.h"

using namespace std;

typedef PHIODataNode<CglTrack> MyNode_t;

DumpCglTrack::DumpCglTrack(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpCglTrack::process_Node(PHNode *myNode)
{
  CglTrack *cgltrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      cgltrack = thisNode->getData();
    }
  if (cgltrack && cgltrack->isValid())
    {
      cgltrack->ShutUp();
      *fout << "CGL get_CglNTrack(): " << cgltrack->get_CglNTrack() << endl;
      for (unsigned int i = 0; i < cgltrack->get_CglNTrack(); i++)
        {
          *fout << "cgltrack->get_id(" << i << "): " << cgltrack->get_id(i) << endl;
          *fout << "cgltrack->get_arm(" << i << "): " << cgltrack->get_arm(i) << endl;
          *fout << "cgltrack->get_accrecid(" << i << "): " << cgltrack->get_accrecid(i) << endl;
          *fout << "cgltrack->get_dctracksid(" << i << "): " << cgltrack->get_dctracksid(i) << endl;
          *fout << "cgltrack->get_emcclusid(" << i << "): " << cgltrack->get_emcclusid(i) << endl;
          *fout << "cgltrack->get_hbdblobid(" << i << "): " << cgltrack->get_hbdblobid(i) << endl;
          *fout << "cgltrack->get_mrpcrecid(" << i << "): " << cgltrack->get_mrpcrecid(i) << endl;
          *fout << "cgltrack->get_pc1clusid(" << i << "): " << cgltrack->get_pc1clusid(i) << endl;
          *fout << "cgltrack->get_pc2clusid(" << i << "): " << cgltrack->get_pc2clusid(i) << endl;
          *fout << "cgltrack->get_pc3clusid(" << i << "): " << cgltrack->get_pc3clusid(i) << endl;
          *fout << "cgltrack->get_pcrrecid(" << i << "): " << cgltrack->get_pcrrecid(i) << endl;
          *fout << "cgltrack->get_tectrackid(" << i << "): " << cgltrack->get_tectrackid(i) << endl;
          *fout << "cgltrack->get_tofrecid(" << i << "): " << cgltrack->get_tofrecid(i) << endl;
          *fout << "cgltrack->get_tofwrecid(" << i << "): " << cgltrack->get_tofwrecid(i) << endl;
          *fout << "cgltrack->get_tzrrecid(" << i << "): " << cgltrack->get_tzrrecid(i) << endl;
          *fout << "cgltrack->get_richringid(" << i << "): " << cgltrack->get_richringid(i) << endl;
          *fout << "cgltrack->get_trackModel(" << i << "): " << cgltrack->get_trackModel(i) << endl;
          *fout << "cgltrack->get_quality(" << i << "): " << cgltrack->get_quality(i) << endl;
	  for (int j = 0; j<4;j++)
	    {
	      *fout << "cgltrack->get_svxclusid(" << i << "," << j << "): " << cgltrack->get_svxclusid(i,j) << endl;
	    }
	}
    }
  return 0;
}

