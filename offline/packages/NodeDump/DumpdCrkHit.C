//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dCrkHitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdCrkHit.h"

using namespace std;

typedef PHIODataNode<dCrkHitWrapper> MyNode_t;

DumpdCrkHit::DumpdCrkHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdCrkHit::process_Node(PHNode *myNode)
{
  dCrkHitWrapper *dcrkhit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dcrkhit = thisNode->getData();
    }
  if (dcrkhit && dcrkhit->RowCount())
    {
      *fout << "get_dCrkNHit(): " << dcrkhit->RowCount() << endl;
      for (unsigned int i = 0; i < dcrkhit->RowCount(); i++)
        {
          *fout << "get_pmt(" << i << "): " << dcrkhit->get_pmt(i) << endl;
          *fout << "get_npe(" << i << "): " << dcrkhit->get_npe(i) << endl;
          *fout << "get_time(" << i << "): " << dcrkhit->get_time(i) << endl;
        }
    }
  return 0;
}

