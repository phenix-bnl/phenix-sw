//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "CrkRing.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpCrkRing.h"

using namespace std;

typedef PHIODataNode<CrkRing> MyNode_t;

DumpCrkRing::DumpCrkRing(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpCrkRing::process_Node(PHNode *myNode)
{
  CrkRing *crkring = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      crkring = thisNode->getData();
    }
  if (crkring && crkring->isValid())
    {
      *fout << "get_CrkNRing(): " << crkring->get_CrkNRing() << endl;
      for (unsigned int i = 0; i < crkring->get_CrkNRing(); i++)
        {
          *fout << "get_panel(" << i << "): " << crkring->get_panel(i) << endl;
          *fout << "get_accepted(" << i << "): " << crkring->get_accepted(i) << endl;
          *fout << "get_npmt0(" << i << "): " << crkring->get_npmt0(i) << endl;
          *fout << "get_npmt1(" << i << "): " << crkring->get_npmt1(i) << endl;
          *fout << "get_npmt2(" << i << "): " << crkring->get_npmt2(i) << endl;
          *fout << "get_npmt3(" << i << "): " << crkring->get_npmt3(i) << endl;
          *fout << "get_npe0(" << i << "): " << crkring->get_npe0(i) << endl;
          *fout << "get_npe1(" << i << "): " << crkring->get_npe1(i) << endl;
          *fout << "get_npe2(" << i << "): " << crkring->get_npe2(i) << endl;
          *fout << "get_npe3(" << i << "): " << crkring->get_npe3(i) << endl;
          *fout << "get_chi2(" << i << "): " << crkring->get_chi2(i) << endl;
          *fout << "get_disp(" << i << "): " << crkring->get_disp(i) << endl;
          *fout << "get_cross_phi(" << i << "): " << crkring->get_cross_phi(i) << endl;
          *fout << "get_cross_z(" << i << "): " << crkring->get_cross_z(i) << endl;
          *fout << "get_center_phi(" << i << "): " << crkring->get_center_phi(i) << endl;
          *fout << "get_center_z(" << i << "): " << crkring->get_center_z(i) << endl;
          *fout << "get_tcrk(" << i << "): " << crkring->get_tcrk(i) << endl;
          *fout << "get_cgltrackid(" << i << "): " << crkring->get_cgltrackid(i) << endl;
        }
    }
  return 0;
}

