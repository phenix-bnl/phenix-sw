#include <DumpTecClusterContainer.h>
#include <TecClusterContainer.hh>
#include <TecCluster.hh>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<TecClusterContainer> MyNode_t;

DumpTecClusterContainer::DumpTecClusterContainer(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpTecClusterContainer::process_Node(PHNode *myNode)
{
  TecClusterContainer *tecclustercontainer = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tecclustercontainer = thisNode->getData();
    }
  //  if (tecclustercontainer && tecclustercontainer->isValid())
  if (tecclustercontainer)
    {
      *fout << "tecclustercontainer->getNClusters(): " << tecclustercontainer->getNClusters() << endl;
      for (int i = 0; i < tecclustercontainer->getNClusters(); i++)
        {
          TecCluster *teccluster = tecclustercontainer->getTecCluster(i);
          *fout << "teccluster->get_side(): " << teccluster->get_side() << endl;
          *fout << "teccluster->get_sector(): " << teccluster->get_sector() << endl;
          *fout << "teccluster->get_plane(): " << teccluster->get_plane() << endl;
          *fout << "teccluster->get_index(): " << teccluster->get_index() << endl;
          *fout << "teccluster->get_wire(): " << teccluster->get_wire() << endl;
          *fout << "teccluster->get_avgtime(): " << teccluster->get_avgtime() << endl;
          *fout << "teccluster->get_ntimebins(): " << teccluster->get_ntimebins() << endl;
        }
    }
  return 0;
}

