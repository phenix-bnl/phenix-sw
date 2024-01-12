//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofGhitRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofGhitRaw.h"

using namespace std;

typedef PHIODataNode<dTofGhitRawWrapper> MyNode_t;

DumpdTofGhitRaw::DumpdTofGhitRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofGhitRaw::process_Node(PHNode *myNode)
{
  dTofGhitRawWrapper *dtofghitraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofghitraw = thisNode->getData();
    }
  if (dtofghitraw && dtofghitraw->RowCount())
    {
      *fout << "RowCount(): " << dtofghitraw->RowCount() << endl;
      for (unsigned int i = 0; i < dtofghitraw->RowCount(); i++)
        {
          *fout << "tofghitraw->get_ghitid(" << i << "): " << dtofghitraw->get_ghitid(i) << endl;
          *fout << "tofghitslat->get_slatid(" << i << "): " << dtofghitraw->get_slatid(i) << endl;
          *fout << "tofghitraw->get_rawid(" << i << "): " << dtofghitraw->get_rawid(i) << endl;
        }
    }
  return 0;
}

