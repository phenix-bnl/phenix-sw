#include "DumpdEmcGeaHit.h"

#include "dEmcGeaHitWrapper.h"

#include "PHIODataNode.h"

#include <iostream>
#include <string>

using namespace std;

typedef PHIODataNode<dEmcGeaHitWrapper> MyNode_t;

DumpdEmcGeaHit::DumpdEmcGeaHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcGeaHit::process_Node(PHNode *myNode)
{
  dEmcGeaHitWrapper *demcgea = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demcgea = thisNode->getData();
    }
  if (demcgea)
    {
      *fout << "RowCount(): " << demcgea->RowCount() << endl;
      for (unsigned int i = 0; i < demcgea->RowCount(); i++)
        {
	  *fout << "get_id(" << i << "): " << demcgea->get_id(i) << endl;
	  *fout << "get_type(" << i << "): " << demcgea->get_type(i) << endl;
	  *fout << "get_sector(" << i << "): " << demcgea->get_sector(i) << endl;
	  *fout << "get_smodind(" << i << "): " << demcgea->get_smodind(i) << endl;
	  *fout << "get_towerind(" << i << "): " << demcgea->get_towerind(i) << endl;
	  *fout << "get_deltae(" << i << "): " << demcgea->get_deltae(i) << endl;
	  *fout << "get_tof(" << i << "): " << demcgea->get_tof(i) << endl;
	  *fout << "get_numed(" << i << "): " << demcgea->get_numed(i) << endl;
	  *fout << "get_partid(" << i << "): " << demcgea->get_partid(i) << endl;
	  *fout << "get_itrack(" << i << "): " << demcgea->get_itrack(i) << endl;
	  *fout << "get_isubevt(" << i << "): " << demcgea->get_isubevt(i) << endl;
	  *fout << "get_nfile(" << i << "): " << demcgea->get_nfile(i) << endl;
	  *fout << "get_true_track(" << i << "): " << demcgea->get_true_track(i) << endl;
	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_xyz(" << j << "," << i << "): " << demcgea->get_xyz(j,i) << endl;
	    }
        }
    }
  return 0;
}

