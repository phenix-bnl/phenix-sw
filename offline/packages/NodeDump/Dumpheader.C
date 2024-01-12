//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "headerWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumpheader.h"

using namespace std;

typedef PHIODataNode<headerWrapper> MyNode_t;

Dumpheader::Dumpheader(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumpheader::process_Node(PHNode *myNode)
{
  headerWrapper *header = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      header = thisNode->getData();
    }
  if (header)
    {
      *fout << "RowCount(): " << header->RowCount() << endl;
      for (unsigned int i = 0; i < header->RowCount(); i++)
        {
	  *fout << "get_run(" << i << "): " << header->get_run(i) << endl;
	  *fout << "get_event(" << i << "): " << header->get_event(i) << endl;
	  *fout << "get_multiplicity(" << i << "): " << header->get_multiplicity(i) << endl;
	  *fout << "get_b(" << i << "): " << header->get_b(i) << endl;
	  *fout << "get_a1(" << i << "): " << header->get_a1(i) << endl;
	  *fout << "get_z1(" << i << "): " << header->get_z1(i) << endl;
	  *fout << "get_a2(" << i << "): " << header->get_a2(i) << endl;
	  *fout << "get_z2(" << i << "): " << header->get_z2(i) << endl;
	  *fout << "get_sqrt_s(" << i << "): " << header->get_sqrt_s(i) << endl;
	  *fout << "get_bmin(" << i << "): " << header->get_bmin(i) << endl;
	  *fout << "get_bmax(" << i << "): " << header->get_bmax(i) << endl;
	  *fout << "get_t0femto(" << i << "): " << header->get_t0femto(i) << endl;
	  *fout << "get_itime(" << i << "): " << header->get_itime(i) << endl;
	  *fout << "get_idate(" << i << "): " << header->get_idate(i) << endl;
	  *fout << "get_isqStart(" << i << "): " << header->get_isqStart(i) << endl;
	  *fout << "get_iSeconds(" << i << "): " << header->get_iSeconds(i) << endl;
	  *fout << "get_maxTrueTrack(" << i << "): " << header->get_maxTrueTrack(i) << endl;
	  for (short j = 0;j<3;j++)
	    {
	      *fout << "get_vertex(" << j << "," << i << "): " << header->get_vertex(j,i) << endl;
	    }
	  for (short j = 0;j<2;j++)
	    {
	      *fout << "get_nrndm(" << j << "," << i << "): " << header->get_nrndm(j,i) << endl;
	    }
        }
    }
  return 0;
}

