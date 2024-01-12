
#include "PHIODataNode.h"
#include "PHNode.h"

#include "crkghitWrapper.h"

#include "Dumpcrkghit.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<crkghitWrapper> MyNode_t;

Dumpcrkghit::Dumpcrkghit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumpcrkghit::process_Node(PHNode *myNode)
{
  crkghitWrapper *crkghit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      crkghit = thisNode->getData();
    }
  if (crkghit && crkghit->RowCount())
    {
      *fout << "get_crkNGhit(): " << crkghit->RowCount() << endl;
      for (unsigned int i = 0; i < crkghit->RowCount(); i++)
	{
          *fout << "crkghit->get_x(" << i << "): " << crkghit->get_x(i) << endl;
          *fout << "crkghit->get_y(" << i << "): " << crkghit->get_y(i) << endl;
          *fout << "crkghit->get_z(" << i << "): " << crkghit->get_z(i) << endl;
          *fout << "crkghit->get_px(" << i << "): " << crkghit->get_px(i) << endl;
          *fout << "crkghit->get_py(" << i << "): " << crkghit->get_py(i) << endl;
          *fout << "crkghit->get_pz(" << i << "): " << crkghit->get_pz(i) << endl;
          *fout << "crkghit->get_tof(" << i << "): " << crkghit->get_tof(i) << endl;
          *fout << "crkghit->get_bp1(" << i << "): " << crkghit->get_bp1(i) << endl;
          *fout << "crkghit->get_bp2(" << i << "): " << crkghit->get_bp2(i) << endl;
          *fout << "crkghit->get_pid(" << i << "): " << crkghit->get_pid(i) << endl;
          *fout << "crkghit->get_parent(" << i << "): " << crkghit->get_parent(i) << endl;
          *fout << "crkghit->get_pmt(" << i << "): " << crkghit->get_pmt(i) << endl;
          *fout << "crkghit->get_tra(" << i << "): " << crkghit->get_tra(i) << endl;
          *fout << "crkghit->get_nbf(" << i << "): " << crkghit->get_nbf(i) << endl;
          *fout << "crkghit->get_bi1(" << i << "): " << crkghit->get_bi1(i) << endl;
          *fout << "crkghit->get_bi2(" << i << "): " << crkghit->get_bi2(i) << endl;
	}
    }
  return 0;
}

