//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "AccCluster.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpAccCluster.h"
#include "AccSnglCluster.h"

using namespace std;

typedef PHIODataNode<AccCluster> MyNode_t;

DumpAccCluster::DumpAccCluster(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpAccCluster::process_Node(PHNode *myNode)
{
  AccCluster *acccluster = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      acccluster = thisNode->getData();
    }
  if (acccluster && acccluster->isValid())
    {
      *fout << "AccCluster->get_ncluster: " << acccluster->get_ncluster() << endl;
      for (short int i = 0; i < acccluster->get_ncluster(); i++)
        {
          AccSnglCluster *sngl = acccluster->get_cluster(i);
          for (int j = 0; j < 4;j++)
            {
              *fout << "AccSnglCluster(" << i << ")->get_aerph1(" << j << "): " << sngl->get_aerph1(j) << endl;
              *fout << "AccSnglCluster(" << i << ")->get_aerph2(" << j << "): " << sngl->get_aerph2(j) << endl;
              *fout << "AccSnglCluster(" << i << ")->get_aert1(" << j << "): " << sngl->get_aert1(j) << endl;
              *fout << "AccSnglCluster(" << i << ")->get_aert2(" << j << "): " << sngl->get_aert2(j) << endl;
            }
          *fout << "AccSnglCluster(" << i << ")->get_aerhitid(): " << sngl->get_aerhitid() << endl;
          *fout << "AccSnglCluster(" << i << ")->get_aerhitconfig(): " << sngl->get_aerhitconfig() << endl;
        }
    }
  return 0;
}
