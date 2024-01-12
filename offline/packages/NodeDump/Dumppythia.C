//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "pythiaWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumppythia.h"

using namespace std;

typedef PHIODataNode<pythiaWrapper> MyNode_t;

Dumppythia::Dumppythia(const string &NodeName) : DumpObject(NodeName)
{
  return ;
}

int Dumppythia::process_Node(PHNode *myNode)
{
  pythiaWrapper *pythia = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      pythia = thisNode->getData();
    }
  if (pythia && pythia->RowCount())
    {
      *fout << "pythia->RowCount(): " << pythia->RowCount() << endl;
      for (unsigned int i = 0; i < pythia->RowCount(); i++)
        {
          *fout << "pythia->get_pyth_proc_id(" << i << "): " << pythia->get_pyth_proc_id(i) << endl;
          *fout << "pythia->get_pyth_qsqr(" << i << "): " << pythia->get_pyth_qsqr(i) << endl;
          *fout << "pythia->get_pyth_ptrans(" << i << "): " << pythia->get_pyth_ptrans(i) << endl;
          for (short j = 0;j < 2;j++)
            {
              *fout << "pythia->get_pyth_bjork(" << j << "," << i << "): " << pythia->get_pyth_bjork(j, i) << endl;
            }
          for (short j = 0;j < 3;j++)
            {
              *fout << "pythia->get_pyth_parstu(" << j << "," << i << "): " << pythia->get_pyth_parstu(j, i) << endl;
            }
          for (short j = 0;j < 4;j++)
            {
              *fout << "pythia->get_intr_part_id(" << j << "," << i << "): " << pythia->get_intr_part_id(j, i) << endl;

              for (short k = 0;k < 4;k++)
                {
                  *fout << "pythia->get_intr_part_p(" << k << "," << j << "," << i << "): " << pythia->get_intr_part_p(k, j, i) << endl;
                }
            }
        }
    }
  return 0;
}

