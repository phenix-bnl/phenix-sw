//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dPadGhitRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdPadGhitRaw.h"

using namespace std;

typedef PHIODataNode<dPadGhitRawWrapper> MyNode_t;

DumpdPadGhitRaw::DumpdPadGhitRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdPadGhitRaw::process_Node(PHNode *myNode)
{
  dPadGhitRawWrapper *dpadghitraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dpadghitraw = thisNode->getData();
    }
  if (dpadghitraw && dpadghitraw->RowCount())
    {
      *fout << "RowCount(): " << dpadghitraw->RowCount() << endl;
      for (unsigned int i = 0; i < dpadghitraw->RowCount(); i++)
        {
          *fout << "padghitraw->get_ghitid(" << i << "): " << dpadghitraw->get_ghitid(i) << endl;
          *fout << "padghitraw->get_rawid(" << i << "): " << dpadghitraw->get_rawid(i) << endl;
        }
    }
  return 0;
}

