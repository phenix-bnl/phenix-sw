#include "DumpAerGeaHits.h"

#include <AerGeaHits.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<AerGeaHits> MyNode_t;

DumpAerGeaHits::DumpAerGeaHits(const string &NodeName) : DumpObject(NodeName)
{
  return ;
}

int DumpAerGeaHits::process_Node(PHNode *myNode)
{
  AerGeaHits *aergeahit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      aergeahit = thisNode->getData();
    }
  if (aergeahit && aergeahit->isValid())
    {
       *fout << "get_nhits(): " << aergeahit->get_nhits() << endl;
//       for (unsigned int i = 0; i < aergeahit->RowCount(); i++)
//         {
//           *fout << "aergeahit->get_del(" << i << "): " << aergeahit->get_del(i) << endl;
//           *fout << "aergeahit->get_tof(" << i << "): " << aergeahit->get_tof(i) << endl;
//           *fout << "aergeahit->get_len(" << i << "): " << aergeahit->get_len(i) << endl;
//           *fout << "aergeahit->get_pmt(" << i << "): " << aergeahit->get_pmt(i) << endl;
//           *fout << "aergeahit->get_pid(" << i << "): " << aergeahit->get_pid(i) << endl;
//           *fout << "aergeahit->get_mctrack(" << i << "): " << aergeahit->get_mctrack(i) << endl;
//           for (short j = 0;j < 3;j++)
//             {
//               *fout << "aergeahit->get_pos(" << j << "," << i << "): " << aergeahit->get_pos(j, i) << endl;
//               *fout << "aergeahit->get_mom(" << j << "," << i << "): " << aergeahit->get_mom(j, i) << endl;
//             }
//         }
    }
  return 0;
}

