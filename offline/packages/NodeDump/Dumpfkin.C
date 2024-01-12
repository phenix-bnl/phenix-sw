//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "fkinWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "Dumpfkin.h"

using namespace std;

typedef PHIODataNode<fkinWrapper> MyNode_t;

Dumpfkin::Dumpfkin(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int Dumpfkin::process_Node(PHNode *myNode)
{
  fkinWrapper *fkin = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      fkin = thisNode->getData();
    }
  if (fkin)
    {
      *fout << "RowCount(): " << fkin->RowCount() << endl;
      for (unsigned int i = 0; i < fkin->RowCount(); i++)
        {
	  *fout << "get_true_track(" << i << "): " << fkin->get_true_track(i) << endl;
	  *fout << "get_subevent(" << i << "): " << fkin->get_subevent(i) << endl;
	  *fout << "get_ntrack(" << i << "): " << fkin->get_ntrack(i) << endl;
	  *fout << "get_ptot(" << i << "): " << fkin->get_ptot(i) << endl;
	  *fout << "get_pthet(" << i << "): " << fkin->get_pthet(i) << endl;
	  *fout << "get_pphi(" << i << "): " << fkin->get_pphi(i) << endl;
	  *fout << "get_r_vertex(" << i << "): " << fkin->get_r_vertex(i) << endl;
	  *fout << "get_z_vertex(" << i << "): " << fkin->get_z_vertex(i) << endl;
	  *fout << "get_th_vertx(" << i << "): " << fkin->get_th_vertx(i) << endl;
	  *fout << "get_ph_vertx(" << i << "): " << fkin->get_ph_vertx(i) << endl;
	  *fout << "get_itparent(" << i << "): " << fkin->get_itparent(i) << endl;
	  *fout << "get_idparent(" << i << "): " << fkin->get_idparent(i) << endl;
	  *fout << "get_idpart(" << i << "): " << fkin->get_idpart(i) << endl;
	  *fout << "get_nfile(" << i << "): " << fkin->get_nfile(i) << endl;
        }
    }
  return 0;
}

