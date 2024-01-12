#include "DumpSvxClusterList.h"

#include <SvxClusterList.h>
#include <SvxCluster.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<SvxClusterList> MyNode_t;

DumpSvxClusterList::DumpSvxClusterList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxClusterList::process_Node(PHNode *myNode)
{
  SvxClusterList *svxclusterlist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxclusterlist = thisNode->getData();
    }
  if (svxclusterlist && svxclusterlist->isValid())
    {
      *fout << "svxclusterlist->get_nClusters(): " << svxclusterlist->get_nClusters() << endl;
      for (int i = 0; i < svxclusterlist->get_nClusters(); i++)
        {
          SvxCluster *raw = svxclusterlist->get_Cluster(i);
          *fout << "svxcluster->get_hitID(" << i << "): " << raw->get_hitID() << endl;
          *fout << "svxcluster->get_svxSection(" << i << "): " << raw->get_svxSection() << endl;
          *fout << "svxcluster->get_layer(" << i << "): " << raw->get_layer() << endl;
          *fout << "svxcluster->get_ladder(" << i << "): " << raw->get_ladder() << endl;
          *fout << "svxcluster->get_sensor(" << i << "): " << raw->get_sensor() << endl;
          *fout << "svxcluster->get_sensorType(" << i << "): " << raw->get_sensorType() << endl;
	  *fout << "svxcluster->get_size(" << i << "): " << raw->get_size() << endl;
	  *fout << "svxcluster->get_AssociatedCGL(" << i << "): " << raw->get_AssociatedCGL() << endl;
	  *fout << "svxcluster->get_AssociatedStandalone(" << i << "): " << raw->get_AssociatedStandalone() << endl;
	  for (int j = 0; j < 2; j++)
	    {
	      *fout << "svxcluster->get_adc(" << i <<  "," << j << "): " << raw->get_adc(j) << endl;
	      *fout << "svxcluster->get_xz_size(" << i << "," << j << "): " << raw->get_xz_size(j) << endl;
	    }
	  for (int j = 0; j < 3; j++)
	    {
	      *fout << "svxcluster->get_xyz_local(" << i <<  "," << j << "): " << raw->get_xyz_local(j) << endl;
	      *fout << "svxcluster->get_xyz_global(" << i <<  "," << j << "): " << raw->get_xyz_global(j) << endl;
	    }
        }
    }
  return 0;
}

