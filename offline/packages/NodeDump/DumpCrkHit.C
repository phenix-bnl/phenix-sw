//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "CrkHit.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpCrkHit.h"

using namespace std;

typedef PHIODataNode<CrkHit> MyNode_t;

DumpCrkHit::DumpCrkHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpCrkHit::process_Node(PHNode *myNode)
{
  CrkHit *crkhit = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      crkhit = thisNode->getData();
    }
  if (crkhit && crkhit->isValid())
    {
      *fout << "get_CrkNHit(): " << crkhit->get_CrkNHit() << endl;
      for (unsigned int i = 0; i < crkhit->get_CrkNHit(); i++)
        {
          *fout << "get_pmt(" << i << "): " << crkhit->get_pmt(i) << endl;
          *fout << "get_npe(" << i << "): " << crkhit->get_npe(i) << endl;
          *fout << "get_time(" << i << "): " << crkhit->get_time(i) << endl;
        }
    }
  return 0;
}

