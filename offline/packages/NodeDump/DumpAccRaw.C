//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "AccRaw.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpAccRaw.h"

using namespace std;

typedef PHIODataNode<AccRaw> MyNode_t;

DumpAccRaw::DumpAccRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpAccRaw::process_Node(PHNode *myNode)
{
  AccRaw *accraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      accraw = thisNode->getData();
    }
  if (accraw && accraw->isValid())
    {
      *fout << "AccRaw->get_nraw: " << accraw->get_nraw() << endl;
      for (short int i = 0; i < accraw->get_nraw(); i++)
        {
          *fout << "AccRaw->get_boxid(" << i << "): " << accraw->get_boxid(i) << endl;
          for (short int j = 0; j < 2;j++)
            {
              *fout << "AccRaw->get_adc(" << i << "," << j << "): " << accraw->get_adc(i, j) << endl;
              *fout << "AccRaw->get_tdc(" << i << "," << j << "): " << accraw->get_tdc(i, j) << endl;
              *fout << "AccRaw->get_adcpost(" << i << "," << j << "): " << accraw->get_adcpost(i, j) << endl;
              *fout << "AccRaw->get_adcpre(" << i << "," << j << "): " << accraw->get_adcpre(i, j) << endl;
            }
        }

    }
  return 0;
}
