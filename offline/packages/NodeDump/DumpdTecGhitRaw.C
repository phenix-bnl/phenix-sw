#include "DumpdTecGhitRaw.h"

#include "dTecGhitRawWrapper.h"

#include "PHIODataNode.h"
#include "PHNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<dTecGhitRawWrapper> MyNode_t;

DumpdTecGhitRaw::DumpdTecGhitRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTecGhitRaw::process_Node(PHNode *myNode)
{
  dTecGhitRawWrapper *dtecghitraw = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtecghitraw = thisNode->getData();
    }
  if (dtecghitraw && dtecghitraw->RowCount())
    {
      *fout << "RowCount(): " << dtecghitraw->RowCount() << endl;
      for (unsigned int i = 0; i < dtecghitraw->RowCount(); i++)
        {
          *fout << "tecghitraw->get_ghitid(" << i << "): " << dtecghitraw->get_ghitid(i) << endl;
          *fout << "tecghitraw->get_rawid(" << i << "): " << dtecghitraw->get_rawid(i) << endl;
          *fout << "tecghitraw->get_binnum(" << i << "): " << dtecghitraw->get_binnum(i) << endl;
          *fout << "tecghitraw->get_fraction(" << i << "): " << dtecghitraw->get_fraction(i) << endl;
        }
    }
  return 0;
}

