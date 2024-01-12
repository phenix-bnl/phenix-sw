//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "TrigLvl1.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpTrigLvl1.h"

using namespace std;

typedef PHIODataNode<TrigLvl1> MyNode_t;

DumpTrigLvl1::DumpTrigLvl1(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpTrigLvl1::process_Node(PHNode *myNode)
{
  TrigLvl1 *triglvl1 = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      triglvl1 = thisNode->getData();
    }
  if (triglvl1)
    {
      *fout << "isValid(): " << triglvl1->isValid() << endl;
      if (triglvl1->isValid())
        {
          *fout << "get_lvl1_trigraw(): 0x" << hex << triglvl1->get_lvl1_trigraw() << dec << endl;
          *fout << "get_lvl1_triglive(): 0x" << hex << triglvl1->get_lvl1_triglive() << dec << endl;
          *fout << "get_lvl1_trigscaled(): 0x" << hex << triglvl1->get_lvl1_trigscaled() << dec << endl;
          *fout << "get_lvl1_clock_cross(): 0x" << hex << triglvl1->get_lvl1_clock_cross() << dec << endl;
          for (unsigned short i = 0;i < 5;i++)
            {
              *fout << "get_lvl1_rbits(" << i << "): 0x" << hex << triglvl1->get_lvl1_rbits(i) << dec << endl;
            }
        }
    }
  return 0;
}
