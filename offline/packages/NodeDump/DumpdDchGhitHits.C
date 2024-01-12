#include "DumpdDchGhitHits.h"

#include "dDchGhitHitsWrapper.h"

#include "PHIODataNode.h"
#include "PHNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<dDchGhitHitsWrapper> MyNode_t;

DumpdDchGhitHits::DumpdDchGhitHits(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdDchGhitHits::process_Node(PHNode *myNode)
{
  dDchGhitHitsWrapper *ddchghithits = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      ddchghithits = thisNode->getData();
    }
  if (ddchghithits && ddchghithits->RowCount())
    {
      *fout << "RowCount(): " << ddchghithits->RowCount() << endl;
      for (unsigned int i = 0; i < ddchghithits->RowCount(); i++)
        {
          *fout << "dchghithits->get_ghitid(" << i << "): " << ddchghithits->get_ghitid(i) << endl;
          *fout << "dchghithits->get_hitsid(" << i << "): " << ddchghithits->get_hitsid(i) << endl;
        }
    }
  return 0;
}

