//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dBbcRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdBbcRaw.h"

using namespace std;

typedef PHIODataNode<dBbcRawWrapper> MyNode_t;

DumpdBbcRaw::DumpdBbcRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdBbcRaw::process_Node(PHNode *myNode)
{
  dBbcRawWrapper *dbbcraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dbbcraw = thisNode->getData();
    }
  if (dbbcraw && dbbcraw->RowCount())
    {
      *fout << "RowCount(): " << dbbcraw->RowCount() << endl;
      for (unsigned int i = 0; i < dbbcraw->RowCount(); i++)
        {
          *fout << "bbcraw->get_Pmt(" << i << "): " << dbbcraw->get_Pmt(i) << endl;
          *fout << "bbcraw->get_Arm(" << i << "): " << dbbcraw->get_Arm(i) << endl;
          *fout << "bbcraw->get_Half(" << i << "): " << dbbcraw->get_Half(i) << endl;
          *fout << "bbcraw->get_Ring(" << i << "): " << dbbcraw->get_Ring(i) << endl;
          *fout << "bbcraw->get_Tube(" << i << "): " << dbbcraw->get_Tube(i) << endl;
          *fout << "bbcraw->get_Adc(" << i << "): " << dbbcraw->get_Adc(i) << endl;
          *fout << "bbcraw->get_Tdc0(" << i << "): " << dbbcraw->get_Tdc0(i) << endl;
          *fout << "bbcraw->get_Tdc1(" << i << "): " << dbbcraw->get_Tdc1(i) << endl;
        }
    }
  return 0;
}

