//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcCalibTowerWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcCalibTower.h"

using namespace std;

typedef PHIODataNode<dEmcCalibTowerWrapper> MyNode_t;

DumpdEmcCalibTower::DumpdEmcCalibTower(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcCalibTower::process_Node(PHNode *myNode)
{
  dEmcCalibTowerWrapper *demccalib = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demccalib = thisNode->getData();
    }
  if (demccalib)
    {
      *fout << "RowCount(): " << demccalib->RowCount() << endl;
      for (unsigned int i = 0; i < demccalib->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << demccalib->get_id(i) << endl;
	  *fout << "get_hwkey(" << i << "): " << demccalib->get_hwkey(i) << endl;
	  *fout << "get_swkey(" << i << "): " << demccalib->get_swkey(i) << endl;
	  *fout << "get_type(" << i << "): " << demccalib->get_type(i) << endl;
	  *fout << "get_arm(" << i << "): " << demccalib->get_arm(i) << endl;
	  *fout << "get_sector(" << i << "): " << demccalib->get_sector(i) << endl;
	  *fout << "get_ecal(" << i << "): " << demccalib->get_ecal(i) << endl;
	  *fout << "get_tof(" << i << "): " << demccalib->get_tof(i) << endl;
	  *fout << "get_adc(" << i << "): " << demccalib->get_adc(i) << endl;
	  *fout << "get_tac(" << i << "): " << demccalib->get_tac(i) << endl;
	  *fout << "get_deadmap(" << i << "): " << demccalib->get_deadmap(i) << endl;
	  *fout << "get_warnmap(" << i << "): " << demccalib->get_warnmap(i) << endl;
	  for (short j = 0;j<2;j++)
	    {
	      *fout << "get_ind(" << j << "," << i << "): " << demccalib->get_ind(j,i) << endl;
	    }
        }
    }
  return 0;
}

