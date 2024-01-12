//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dPadNibbleGhitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdPadNibbleGhit.h"

using namespace std;

typedef PHIODataNode<dPadNibbleGhitWrapper> MyNode_t;

DumpdPadNibbleGhit::DumpdPadNibbleGhit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdPadNibbleGhit::process_Node(PHNode *myNode)
{
  dPadNibbleGhitWrapper *dpadnibbleghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dpadnibbleghit = thisNode->getData();
    }
  if (dpadnibbleghit && dpadnibbleghit->RowCount())
    {
      *fout << "get_PadNNibbleGhit(): " << dpadnibbleghit->RowCount() << endl;
      for (unsigned int i = 0; i < dpadnibbleghit->RowCount(); i++)
        {
          *fout << "padnibbleghit->get_ghitid(" << i << "): " << dpadnibbleghit->get_ghitid(i) << endl;
          *fout << "padnibbleghit->get_rawid(" << i << "): " << dpadnibbleghit->get_rawid(i) << endl;
          *fout << "padnibbleghit->get_Card(" << i << "): " << dpadnibbleghit->get_Card(i) << endl;
          *fout << "padnibbleghit->get_padx(" << i << "): " << dpadnibbleghit->get_padx(i) << endl;
          *fout << "padnibbleghit->get_padz(" << i << "): " << dpadnibbleghit->get_padz(i) << endl;
        }
    }
  return 0;
}

