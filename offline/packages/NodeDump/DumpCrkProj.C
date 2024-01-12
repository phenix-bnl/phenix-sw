#include "DumpCrkProj.h"


#include <CrkProj.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<CrkProj> MyNode_t;

DumpCrkProj::DumpCrkProj(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpCrkProj::process_Node(PHNode *myNode)
{
  CrkProj *crkproj = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      crkproj = thisNode->getData();
    }
  if (crkproj && crkproj->isValid())
    {
      *fout << "get_CrkNProj() " << crkproj->get_CrkNProj() << endl;
      for (unsigned int i = 0; i < crkproj->get_CrkNProj(); i++)
	{
	  *fout << "Crk Proj: " << i << endl;
	  *fout << "get_pstartx(" << i << "): " << crkproj->get_pstartx(i) << endl;
	  *fout << "get_pstarty(" << i << "): " << crkproj->get_pstarty(i) << endl;
	  *fout << "get_pstartz(" << i << "): " << crkproj->get_pstartz(i) << endl;
	  *fout << "get_pendx(" << i << "): " << crkproj->get_pendx(i) << endl;
	  *fout << "get_pendy(" << i << "): " << crkproj->get_pendy(i) << endl;
	  *fout << "get_pendz(" << i << "): " << crkproj->get_pendz(i) << endl;
	  *fout << "get_ringid(" << i << "): " << crkproj->get_ringid(i) << endl;
	  *fout << "get_cgltrackid(" << i << "): " << crkproj->get_cgltrackid(i) << endl;
        }
    }
  return 0;
}

