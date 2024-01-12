//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofGhitGdigiWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofGhitGdigi.h"

using namespace std;

typedef PHIODataNode<dTofGhitGdigiWrapper> MyNode_t;

DumpdTofGhitGdigi::DumpdTofGhitGdigi(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofGhitGdigi::process_Node(PHNode *myNode)
{
  dTofGhitGdigiWrapper *dtofghitgdigi = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofghitgdigi = thisNode->getData();
    }
  if (dtofghitgdigi && dtofghitgdigi->RowCount())
    {
      *fout << "RowCount(): " << dtofghitgdigi->RowCount() << endl;
      for (unsigned int i = 0; i < dtofghitgdigi->RowCount(); i++)
        {
          *fout << "tofghitgdigi->get_ghitid(" << i << "): " << dtofghitgdigi->get_ghitid(i) << endl;
          *fout << "tofghitslat->get_slatid(" << i << "): " << dtofghitgdigi->get_slatid(i) << endl;
          *fout << "tofghitgdigi->get_gdigiid(" << i << "): " << dtofghitgdigi->get_gdigiid(i) << endl;
        }
    }
  return 0;
}

