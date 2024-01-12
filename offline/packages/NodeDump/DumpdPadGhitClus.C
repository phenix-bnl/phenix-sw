//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dPadGhitClusWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdPadGhitClus.h"

using namespace std;

typedef PHIODataNode<dPadGhitClusWrapper> MyNode_t;

DumpdPadGhitClus::DumpdPadGhitClus(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdPadGhitClus::process_Node(PHNode *myNode)
{
  dPadGhitClusWrapper *dpadghitclus = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dpadghitclus = thisNode->getData();
    }
  if (dpadghitclus && dpadghitclus->RowCount())
    {
      *fout << "RowCount(): " << dpadghitclus->RowCount() << endl;
      for (unsigned int i = 0; i < dpadghitclus->RowCount(); i++)
        {
          *fout << "padghitclus->get_ghitid(" << i << "): " << dpadghitclus->get_ghitid(i) << endl;
          *fout << "padghitclus->get_clusid(" << i << "): " << dpadghitclus->get_clusid(i) << endl;
        }
    }
  return 0;
}

