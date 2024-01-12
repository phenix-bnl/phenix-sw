//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

//INCLUDECHECKER: Removed this line: #include "Bbc.hh"
#include "BbcRaw.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpBbcRaw.h"

using namespace std;

typedef PHIODataNode<BbcRaw> MyNode_t;

DumpBbcRaw::DumpBbcRaw(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpBbcRaw::process_Node(PHNode *myNode)
{
  BbcRaw *bbcraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      bbcraw = thisNode->getData();
    }
  if (bbcraw && bbcraw->isValid())
    {
      *fout << "BbcRaw->get_npmt: " << bbcraw->get_npmt() << endl;
      for (short int i = 0; i < bbcraw->get_npmt(); i++)
        {
          *fout << "BbcRaw->get_Pmt(" << i << "): " << bbcraw->get_Pmt(i) << endl;
          *fout << "BbcRaw->get_Adc(" << i << "): " << bbcraw->get_Adc(i) << endl;
          *fout << "BbcRaw->get_Tdc0(" << i << "): " << bbcraw->get_Tdc0(i) << endl;
          *fout << "BbcRaw->get_Tdc1(" << i << "): " << bbcraw->get_Tdc1(i) << endl;
        }

    }
  return 0;
}
