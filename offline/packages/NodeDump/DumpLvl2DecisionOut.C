//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "Lvl2DecisionOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpLvl2DecisionOut.h"

using namespace std;

typedef PHIODataNode<Lvl2DecisionOut> MyNode_t;

DumpLvl2DecisionOut::DumpLvl2DecisionOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpLvl2DecisionOut::process_Node(PHNode *myNode)
{
  Lvl2DecisionOut *lvl2decisionout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      lvl2decisionout = thisNode->getData();
    }
  if (lvl2decisionout)
    {
      //      *fout << "isValid(): " << lvl2decisionout->isValid() << endl;
      //if (lvl2decisionout->isValid())
        {
          *fout << "getFullDecision(): 0x" << hex << lvl2decisionout->getFullDecision() << dec << endl;
          *fout << "getNumLevel1Triggers(): 0x" << hex << lvl2decisionout->getNumLevel1Triggers() << dec << endl;
          unsigned int numalg = lvl2decisionout->getMaxNumAlgorithms();
	    *fout << "getMaxNumAlgorithms(): 0x" << numalg << endl;
          unsigned int numlvl1 = lvl2decisionout->getMaxNumLvl1Triggers();
          *fout << "getMaxNumLvl1Triggers(): 0x" << numlvl1 << dec << endl;

          for (unsigned int i = 0;i < numalg ;i++)
            {
              *fout << "getAlgorithmDecision(" << i << "): 0x" << hex << lvl2decisionout->getAlgorithmDecision(i) << dec << endl;
            }
          for (unsigned int i = 0;i < numlvl1 ;i++)
            {
              *fout << "getLevel1TriggerDecision(" << i << "): 0x" << hex << lvl2decisionout->getLevel1TriggerDecision(i) << dec << endl;
              for (unsigned int j = 0;j < numalg ;j++)
                {
                  *fout << "getLvl1AlgorithmDecision(" << i
			<< "," << j << "): 0x" << hex
			<< lvl2decisionout->getLvl1AlgorithmDecision(i, j)
			<< dec << endl;
                }
            }

        }
    }
  return 0;
}
