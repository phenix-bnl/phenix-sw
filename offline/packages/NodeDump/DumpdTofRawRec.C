//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofRawRecWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofRawRec.h"

using namespace std;

typedef PHIODataNode<dTofRawRecWrapper> MyNode_t;

DumpdTofRawRec::DumpdTofRawRec(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofRawRec::process_Node(PHNode *myNode)
{
  dTofRawRecWrapper *dtofrawrec = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofrawrec = thisNode->getData();
    }
  if (dtofrawrec && dtofrawrec->RowCount())
    {
      *fout << "RowCount(): " << dtofrawrec->RowCount() << endl;
      for (unsigned int i = 0; i < dtofrawrec->RowCount(); i++)
        {
          *fout << "tofrawrec->get_rawid(" << i << "): " << dtofrawrec->get_rawid(i) << endl;
          *fout << "tofrawslat->get_slatid(" << i << "): " << dtofrawrec->get_slatid(i) << endl;
          *fout << "tofrawrec->get_recid(" << i << "): " << dtofrawrec->get_recid(i) << endl;
        }
    }
  return 0;
}

