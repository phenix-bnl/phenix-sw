//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "emcClusterContainer.h"
#include "emcClusterContent.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpemcClusterContainer.h"

using namespace std;

typedef PHIODataNode<emcClusterContainer> MyNode_t;

DumpemcClusterContainer::DumpemcClusterContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpemcClusterContainer::process_Node(PHNode *myNode)
{
  emcClusterContainer *emcclustercontainer = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      emcclustercontainer = thisNode->getData();
    }
  if (emcclustercontainer && emcclustercontainer->isValid())
    {
      *fout << "Cluster: " << emcclustercontainer->size() << endl;
      for (unsigned int i = 0; i < emcclustercontainer->size(); i++)
        {
          emcClusterContent* thisclus = emcclustercontainer->getCluster(i);
	  thisclus->ShutUp();
          *fout << "arm(" << i << "): " << thisclus->arm() << endl;
          *fout << "chi2(" << i << "): " << thisclus->chi2() << endl;
          *fout << "deadmap(" << i << "): " << thisclus->deadmap() << endl;
          *fout << "dispy(" << i << "): " << thisclus->dispy() << endl;
          *fout << "dispz(" << i << "): " << thisclus->dispz() << endl;
          *fout << "dx(" << i << "): " << thisclus->dx() << endl;
          *fout << "dy(" << i << "): " << thisclus->dy() << endl;
          *fout << "dz(" << i << "): " << thisclus->dz() << endl;
          *fout << "e(" << i << "): " << thisclus->e() << endl;
          *fout << "e9(" << i << "): " << thisclus->e9() << endl;
          *fout << "ecore(" << i << "): " << thisclus->ecore() << endl;
          *fout << "ecent(" << i << "): " << thisclus->ecent() << endl;
          *fout << "etofmin(" << i << "): " << thisclus->etofmin() << endl;
          *fout << "etofmax(" << i << "): " << thisclus->etofmax() << endl;
          *fout << "id(" << i << "): " << thisclus->id() << endl;
          *fout << "iypos(" << i << "): " << thisclus->iypos() << endl;
          *fout << "izpos(" << i << "): " << thisclus->izpos() << endl;
          *fout << "quality(" << i << "): " << thisclus->quality() << endl;
          *fout << "multiplicity(" << i << "): " << thisclus->multiplicity() << endl;
          *fout << "padispy(" << i << "): " << thisclus->padispy() << endl;
          *fout << "padispz(" << i << "): " << thisclus->padispz() << endl;
          *fout << "prob_photon(" << i << "): " << thisclus->prob_photon() << endl;
          *fout << "phi(" << i << "): " << thisclus->phi() << endl;
          *fout << "pid(" << i << "): " << thisclus->pid() << endl;
          *fout << "sector(" << i << "): " << thisclus->sector() << endl;
          *fout << "tof(" << i << "): " << thisclus->tof() << endl;
          *fout << "tofcorr(" << i << "): " << thisclus->tofcorr() << endl;
          *fout << "tofdisp(" << i << "): " << thisclus->tofdisp() << endl;
          *fout << "tofmin(" << i << "): " << thisclus->tofmin() << endl;
          *fout << "tofmax(" << i << "): " << thisclus->tofmax() << endl;
          *fout << "tofcorrmin(" << i << "): " << thisclus->tofcorrmin() << endl;
          *fout << "tofcorrmax(" << i << "): " << thisclus->tofcorrmax() << endl;
          *fout << "theta(" << i << "): " << thisclus->theta() << endl;
          *fout << "twrhit(" << i << "): " << thisclus->twrhit() << endl;
          *fout << "type(" << i << "): " << thisclus->type() << endl;
          *fout << "warnmap(" << i << "): " << thisclus->warnmap() << endl;
          *fout << "x(" << i << "): " << thisclus->x() << endl;
          *fout << "y(" << i << "): " << thisclus->y() << endl;
          *fout << "z(" << i << "): " << thisclus->z() << endl;
          for (int j = 0; j < thisclus->multiplicity();j++)
            {
              *fout << "partesum(" << i << "," << j << "): " << thisclus->partesum(j) << endl;
              *fout << "towerid(" << i << "," << j << "): " << thisclus->towerid(j) << endl;
            }
        }
    }
  return 0;
}

