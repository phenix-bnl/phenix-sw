#include <string>

#include "PHIODataNode.h"

#include "mpcClusterContainer.h"
#include "mpcClusterContent.h"

#include "DumpmpcClusterContainer.h"

using namespace std;

typedef PHIODataNode<mpcClusterContainer> MyNode_t;

DumpmpcClusterContainer::DumpmpcClusterContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpmpcClusterContainer::process_Node(PHNode *myNode)
{
  mpcClusterContainer *mpcclustercontainer = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mpcclustercontainer = thisNode->getData();
    }
  if (mpcclustercontainer && mpcclustercontainer->isValid())
    {
      *fout << "Clusters: " << mpcclustercontainer->size() << endl;
      for (unsigned int i = 0; i < mpcclustercontainer->size(); i++)
        {
          mpcClusterContent *thiscluster = mpcclustercontainer->getCluster(i);
          *fout << "arm(" << i << "): " << thiscluster->arm() << endl;
          *fout << "chi2(" << i << "): " << thiscluster->chi2() << endl;
          *fout << "corrdispy(" << i << "): " << thiscluster->corrdispy() << endl;
          *fout << "corrdispz(" << i << "): " << thiscluster->corrdispz() << endl;
          *fout << "cutword(" << i << "): " << thiscluster->cutword() << endl;
          *fout << "deadmap(" << i << "): " << thiscluster->deadmap() << endl;
          *fout << "dispy(" << i << "): " << thiscluster->dispy() << endl;
          *fout << "dispz(" << i << "): " << thiscluster->dispz() << endl;
          *fout << "dx(" << i << "): " << thiscluster->dx() << endl;
          *fout << "dy(" << i << "): " << thiscluster->dy() << endl;
          *fout << "dz(" << i << "): " << thiscluster->dz() << endl;
          *fout << "e(" << i << "): " << thiscluster->e() << endl;
          *fout << "e9(" << i << "): " << thiscluster->e9() << endl;
          *fout << "ecore(" << i << "): " << thiscluster->ecore() << endl;
          *fout << "ecent(" << i << "): " << thiscluster->ecent() << endl;
          *fout << "etofmin(" << i << "): " << thiscluster->etofmin() << endl;
          *fout << "etofmax(" << i << "): " << thiscluster->etofmax() << endl;
          *fout << "has_yz_cg(" << i << "): " << thiscluster->has_yz_cg() << endl;
          *fout << "id(" << i << "): " << thiscluster->id() << endl;
          *fout << "ixpos(" << i << "): " << thiscluster->ixpos() << endl;
          *fout << "iypos(" << i << "): " << thiscluster->iypos() << endl;
          *fout << "quality(" << i << "): " << thiscluster->quality() << endl;
          *fout << "multiplicity(" << i << "): " << thiscluster->multiplicity() << endl;
          *fout << "padispy(" << i << "): " << thiscluster->padispy() << endl;
          *fout << "padispz(" << i << "): " << thiscluster->padispz() << endl;
          *fout << "prob_photon(" << i << "): " << thiscluster->prob_photon() << endl;
        }
    }
  return 0;
}

