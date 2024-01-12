//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dTofGdigiWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdTofGdigi.h"

using namespace std;

typedef PHIODataNode<dTofGdigiWrapper> MyNode_t;

DumpdTofGdigi::DumpdTofGdigi(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdTofGdigi::process_Node(PHNode *myNode)
{
  dTofGdigiWrapper *dtofgdigi = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dtofgdigi = thisNode->getData();
    }
  if (dtofgdigi && dtofgdigi->RowCount())
    {
      *fout << "RowCount(): " << dtofgdigi->RowCount() << endl;
      for (unsigned int i = 0; i < dtofgdigi->RowCount(); i++)
        {
          *fout << "tofgdigi->get_id(" << i << "): " << dtofgdigi->get_id(i) << endl;
          *fout << "tofgdigi->get_slatid(" << i << "): " << dtofgdigi->get_slatid(i) << endl;
          *fout << "tofgdigi->get_panel(" << i << "): " << dtofgdigi->get_panel(i) << endl;
          *fout << "tofgdigi->get_column(" << i << "): " << dtofgdigi->get_column(i) << endl;
          *fout << "tofgdigi->get_pslat(" << i << "): " << dtofgdigi->get_pslat(i) << endl;
          *fout << "tofgdigi->get_slat_seq(" << i << "): " << dtofgdigi->get_slat_seq(i) << endl;
          *fout << "tofgdigi->get_mctrack(" << i << "): " << dtofgdigi->get_mctrack(i) << endl;
          *fout << "tofgdigi->get_partl(" << i << "): " << dtofgdigi->get_partl(i) << endl;
          *fout << "tofgdigi->get_tof(" << i << "): " << dtofgdigi->get_tof(i) << endl;
          *fout << "tofgdigi->get_eloss(" << i << "): " << dtofgdigi->get_eloss(i) << endl;
          *fout << "tofgdigi->get_pos_hit_slat(" << i << "): " << dtofgdigi->get_pos_hit_slat(i) << endl;
          *fout << "tofgdigi->get_theta(" << i << "): " << dtofgdigi->get_theta(i) << endl;
          *fout << "tofgdigi->get_phi(" << i << "): " << dtofgdigi->get_phi(i) << endl;
          *fout << "tofgdigi->get_path(" << i << "): " << dtofgdigi->get_path(i) << endl;
          *fout << "tofgdigi->get_nslathit(" << i << "): " << dtofgdigi->get_nslathit(i) << endl;
          *fout << "tofgdigi->get_hits_seq(" << i << "): " << dtofgdigi->get_hits_seq(i) << endl;
	  for (int j = 0; j<3;j++)
	    {
	      *fout << "tofgdigi->get_pos_m(" << j << "," << i << "): " << dtofgdigi->get_pos_m(j,i) << endl;
	      *fout << "tofgdigi->get_p_m(" << j << "," << i << "): " << dtofgdigi->get_p_m(j,i) << endl;
	    }
        }
    }
  return 0;
}

