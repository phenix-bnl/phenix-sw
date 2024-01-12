#include "DumpdDchNibbleGhit.h"

#include "dDchNibbleGhitWrapper.h"

#include "PHIODataNode.h"
#include "PHNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<dDchNibbleGhitWrapper> MyNode_t;

DumpdDchNibbleGhit::DumpdDchNibbleGhit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdDchNibbleGhit::process_Node(PHNode *myNode)
{
  dDchNibbleGhitWrapper *ddchnibbleghit = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      ddchnibbleghit = thisNode->getData();
    }
  if (ddchnibbleghit && ddchnibbleghit->RowCount())
    {
      *fout << "RowCount(): " << ddchnibbleghit->RowCount() << endl;
      for (unsigned int i = 0; i < ddchnibbleghit->RowCount(); i++)
        {
          *fout << "dchnibblghit->get_ghitid(" << i << "): " << ddchnibbleghit->get_ghitid(i) << endl;
          *fout << "dchnibblghit->get_arm(" << i << "): " << ddchnibbleghit->get_arm(i) << endl;
          *fout << "dchnibblghit->get_side(" << i << "): " << ddchnibbleghit->get_side(i) << endl;
          *fout << "dchnibblghit->get_key(" << i << "): " << ddchnibbleghit->get_key(i) << endl;
          *fout << "dchnibblghit->get_pair(" << i << "): " << ddchnibbleghit->get_pair(i) << endl;
          *fout << "dchnibblghit->get_channel(" << i << "): " << ddchnibbleghit->get_channel(i) << endl;
          *fout << "dchnibblghit->get_Nibble(" << i << "): " << ddchnibbleghit->get_Nibble(i) << endl;
        }
    }
  return 0;
}

