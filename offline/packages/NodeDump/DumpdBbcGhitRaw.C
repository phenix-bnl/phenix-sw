#include "DumpdBbcGhitRaw.h"

#include "dBbcGhitRawWrapper.h"

#include "PHIODataNode.h"
#include "PHNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<dBbcGhitRawWrapper> MyNode_t;

DumpdBbcGhitRaw::DumpdBbcGhitRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdBbcGhitRaw::process_Node(PHNode *myNode)
{
  dBbcGhitRawWrapper *dbbcghitraw = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dbbcghitraw = thisNode->getData();
    }
  if (dbbcghitraw && dbbcghitraw->RowCount())
    {
      *fout << "RowCount(): " << dbbcghitraw->RowCount() << endl;
      for (unsigned int i = 0; i < dbbcghitraw->RowCount(); i++)
        {
          *fout << "bbcghitraw->get_ghitid(" << i << "): " << dbbcghitraw->get_ghitid(i) << endl;
          *fout << "bbcghitraw->get_rawid(" << i << "): " << dbbcghitraw->get_rawid(i) << endl;
        }
    }
  return 0;
}

