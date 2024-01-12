//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "bbcghitWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumpbbcghit.h"

using namespace std;

typedef PHIODataNode<bbcghitWrapper> MyNode_t;

Dumpbbcghit::Dumpbbcghit(const string &NodeName) : DumpObject(NodeName)
{
  return ;
}

int Dumpbbcghit::process_Node(PHNode *myNode)
{
  bbcghitWrapper *bbcghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      bbcghit = thisNode->getData();
    }
  if (bbcghit && bbcghit->RowCount())
    {
      *fout << "get_bbcNGhit(): " << bbcghit->RowCount() << endl;
      for (unsigned int i = 0; i < bbcghit->RowCount(); i++)
        {
          *fout << "bbcghit->get_del(" << i << "): " << bbcghit->get_del(i) << endl;
          *fout << "bbcghit->get_tof(" << i << "): " << bbcghit->get_tof(i) << endl;
          *fout << "bbcghit->get_len(" << i << "): " << bbcghit->get_len(i) << endl;
          *fout << "bbcghit->get_pmt(" << i << "): " << bbcghit->get_pmt(i) << endl;
          *fout << "bbcghit->get_pid(" << i << "): " << bbcghit->get_pid(i) << endl;
          *fout << "bbcghit->get_mctrack(" << i << "): " << bbcghit->get_mctrack(i) << endl;
          for (short j = 0;j < 3;j++)
            {
              *fout << "bbcghit->get_pos(" << j << "," << i << "): " << bbcghit->get_pos(j, i) << endl;
              *fout << "bbcghit->get_mom(" << j << "," << i << "): " << bbcghit->get_mom(j, i) << endl;
            }
        }
    }
  return 0;
}

