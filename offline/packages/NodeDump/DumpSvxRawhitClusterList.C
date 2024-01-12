#include "DumpSvxRawhitClusterList.h"

#include <SvxRawhitClusterList.h>
#include <SvxRawhitCluster.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<SvxRawhitClusterList> MyNode_t;

DumpSvxRawhitClusterList::DumpSvxRawhitClusterList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSvxRawhitClusterList::process_Node(PHNode *myNode)
{
  SvxRawhitClusterList *svxrawhitclusterlist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxrawhitclusterlist = thisNode->getData();
    }
  if (svxrawhitclusterlist && svxrawhitclusterlist->isValid())
    {
      *fout << "svxrawhitclusterlist->get_nRawhitClusters(): " << svxrawhitclusterlist->get_nRawhitClusters() << endl;
      for (int i = 0; i < svxrawhitclusterlist->get_nRawhitClusters(); i++)
        {
          SvxRawhitCluster *raw = svxrawhitclusterlist->get_RawhitCluster(i);
          *fout << "svxrawhitcluster->get_rawhitID(" << i << "): " << raw->get_rawhitID() << endl;
          *fout << "svxrawhitcluster->get_clusterID(" << i << "): " << raw->get_clusterID() << endl;
        }
    }
  return 0;
}

