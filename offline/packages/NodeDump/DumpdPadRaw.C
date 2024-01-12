//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dPadRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdPadRaw.h"

using namespace std;

typedef PHIODataNode<dPadRawWrapper> MyNode_t;

DumpdPadRaw::DumpdPadRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdPadRaw::process_Node(PHNode *myNode)
{
  dPadRawWrapper *dpadraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dpadraw = thisNode->getData();
    }
  if (dpadraw && dpadraw->RowCount())
    {
      *fout << "get_PadNRaw(): " << dpadraw->RowCount() << endl;
      for (unsigned int i = 0; i < dpadraw->RowCount(); i++)
        {
          *fout << "padraw->get_arm(" << i << "): " << dpadraw->get_arm(i) << endl;
          *fout << "padraw->get_id(" << i << "): " << dpadraw->get_id(i) << endl;
          *fout << "padraw->get_padtype(" << i << "): " << dpadraw->get_padtype(i) << endl;
          *fout << "padraw->get_padx(" << i << "): " << dpadraw->get_padx(i) << endl;
          *fout << "padraw->get_padz(" << i << "): " << dpadraw->get_padz(i) << endl;
          *fout << "padraw->get_sector(" << i << "): " << dpadraw->get_sector(i) << endl;
          *fout << "padraw->get_side(" << i << "): " << dpadraw->get_side(i) << endl;
        }
    }
  return 0;
}

