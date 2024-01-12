#include "DumpdCrkRel2s.h"

#include "dCrkRel2sWrapper.h"

#include "PHIODataNode.h"
#include "PHNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<dCrkRel2sWrapper> MyNode_t;

DumpdCrkRel2s::DumpdCrkRel2s(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdCrkRel2s::process_Node(PHNode *myNode)
{
  dCrkRel2sWrapper *dcrkrel2s = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dcrkrel2s = thisNode->getData();
    }
  if (dcrkrel2s && dcrkrel2s->RowCount())
    {
      *fout << "RowCount(): " << dcrkrel2s->RowCount() << endl;
      for (unsigned int i = 0; i < dcrkrel2s->RowCount(); i++)
        {
          *fout << "crkrel2s->get_id1(" << i << "): " << dcrkrel2s->get_id1(i) << endl;
          *fout << "crkrel2s->get_id2(" << i << "): " << dcrkrel2s->get_id2(i) << endl;
        }
    }
  return 0;
}

