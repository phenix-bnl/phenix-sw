//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofGdigiRecWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofGdigiRec.h"

using namespace std;

typedef PHIODataNode<dTofGdigiRecWrapper> MyNode_t;

DumpdTofGdigiRec::DumpdTofGdigiRec(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofGdigiRec::process_Node(PHNode *myNode)
{
  dTofGdigiRecWrapper *dtofgdigirec = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofgdigirec = thisNode->getData();
    }
  if (dtofgdigirec && dtofgdigirec->RowCount())
    {
      *fout << "RowCount(): " << dtofgdigirec->RowCount() << endl;
      for (unsigned int i = 0; i < dtofgdigirec->RowCount(); i++)
        {
          *fout << "tofgdigirec->get_gdigiid(" << i << "): " << dtofgdigirec->get_gdigiid(i) << endl;
          *fout << "tofgdigislat->get_slatid(" << i << "): " << dtofgdigirec->get_slatid(i) << endl;
          *fout << "tofgdigirec->get_recid(" << i << "): " << dtofgdigirec->get_recid(i) << endl;
        }
    }
  return 0;
}

