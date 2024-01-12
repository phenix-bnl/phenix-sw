#include "DumpemcClusterAuxInfoContainer.h"

#include <emcClusterAuxInfoContainer.h>
#include <emcClusterAuxInfo.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<emcClusterAuxInfoContainer> MyNode_t;

DumpemcClusterAuxInfoContainer::DumpemcClusterAuxInfoContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpemcClusterAuxInfoContainer::process_Node(PHNode *myNode)
{
  emcClusterAuxInfoContainer *emcClusterAuxInfocontainer = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      emcClusterAuxInfocontainer = thisNode->getData();
    }
  if (emcClusterAuxInfocontainer)
    {
      *fout << "capacity: " << emcClusterAuxInfocontainer->capacity() << endl;
      *fout << "size: " << emcClusterAuxInfocontainer->size() << endl;
      for (unsigned int i = 0; i < emcClusterAuxInfocontainer->size(); i++)
        {
          emcClusterAuxInfo* thisclus = emcClusterAuxInfocontainer->getInfo(i);
          *fout << "getLocalChi2(" << i << "): " << thisclus->getLocalChi2() << endl;
          *fout << "getLocalEcore(" << i << "): " << thisclus->getLocalEcore() << endl;
          *fout << "getLocalx(" << i << "): " << thisclus->getLocalx() << endl;
          *fout << "getLocaly(" << i << "): " << thisclus->getLocaly() << endl;
        }
    }
  return 0;
}

