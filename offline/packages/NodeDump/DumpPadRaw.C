//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "PadRaw.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpPadRaw.h"

using namespace std;

typedef PHIODataNode<PadRaw> MyNode_t;

DumpPadRaw::DumpPadRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPadRaw::process_Node(PHNode *myNode)
{
  PadRaw *padraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      padraw = thisNode->getData();
    }
  if (padraw && padraw->isValid())
    {
      *fout << "get_PadNRaw(): " << padraw->get_PadNRaw() << endl;
      for (unsigned int i = 0; i < padraw->get_PadNRaw(); i++)
        {
          *fout << "padraw->get_arm(" << i << "): " << padraw->get_arm(i) << endl;
          *fout << "padraw->get_id(" << i << "): " << padraw->get_id(i) << endl;
          *fout << "padraw->get_padtype(" << i << "): " << padraw->get_padtype(i) << endl;
          *fout << "padraw->get_padx(" << i << "): " << padraw->get_padx(i) << endl;
          *fout << "padraw->get_padz(" << i << "): " << padraw->get_padz(i) << endl;
          *fout << "padraw->get_sector(" << i << "): " << padraw->get_sector(i) << endl;
          *fout << "padraw->get_side(" << i << "): " << padraw->get_side(i) << endl;
        }
    }
  return 0;
}

