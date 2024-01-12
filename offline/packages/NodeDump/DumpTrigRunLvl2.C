//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "TrigRunLvl2.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpTrigRunLvl2.h"

using namespace std;

typedef PHIODataNode<TrigRunLvl2> MyNode_t;

DumpTrigRunLvl2::DumpTrigRunLvl2(const string &NodeName): DumpObject(NodeName)
{
  write_run_event = 0; // do not write info for each event
  node_written = 0;
  return ;
}

int DumpTrigRunLvl2::process_Node(PHNode *myNode)
{
  TrigRunLvl2 *trigrunlvl2 = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      trigrunlvl2 = thisNode->getData();
    }
  if (!node_written && trigrunlvl2)
    {
      *fout << "trigrunlvl2->isValid(): " << trigrunlvl2->isValid() << endl;
      if (trigrunlvl2->isValid())
        {
          *fout << "get_lvl2_description(): " << trigrunlvl2->get_lvl2_description() << endl;
          *fout << "get_lvl2_version() " << trigrunlvl2->get_lvl2_version() << endl;
          *fout << "get_lvl2_run_enable() " << trigrunlvl2->get_lvl2_run_enable() << endl;
          *fout << "get_lvl2_reject_enable() " << trigrunlvl2->get_lvl2_reject_enable() << endl;
          for (short int i = 0; i < 32 ; i++)
            {
              *fout << "get_lvl1_lvl2_reject_enable(" << i << "): " << trigrunlvl2->get_lvl1_lvl2_reject_enable(i) << endl;
              *fout << "get_lvl1_lvl2_force_accept(" << i << "): " << trigrunlvl2->get_lvl1_lvl2_force_accept(i) << endl;
              for ( short int j = 0; j < 62;j++)
                {
                  *fout << "get_lvl2_lvl1_prescale(" << j << "," << i << "): " << trigrunlvl2->get_lvl2_lvl1_prescale(j, i) << endl;
                  *fout << "get_lvl2_lvl1_assoc(" << j << "," << i << "): " << trigrunlvl2->get_lvl2_lvl1_assoc(j, i) << endl;
                }
            }

          node_written = 1;
        }
    }
  return 0;
}

