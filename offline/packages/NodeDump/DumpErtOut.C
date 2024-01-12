//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "ErtOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpErtOut.h"

using namespace std;

typedef PHIODataNode<ErtOut> MyNode_t;

DumpErtOut::DumpErtOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpErtOut::process_Node(PHNode *myNode)
{
  ErtOut *ertout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      ertout = thisNode->getData();
    }
  if (ertout && ertout->isValid())
    {
      *fout << "get_ERThit_N(): " << ertout->get_ERThit_N() << endl;
      for (int i = 0; i < ertout->get_ERThit_N(); i++)
        {
          *fout << "ErtOut->get_ERTtrigmode(" << i << "): " << ertout->get_ERTtrigmode(i) << endl;
          *fout << "ErtOut->get_ERTarm(" << i << "): " << ertout->get_ERTarm(i) << endl;
          *fout << "ErtOut->get_ERTsector(" << i << "): " << ertout->get_ERTsector(i) << endl;
          *fout << "ErtOut->get_ERTsm(" << i << "): " << ertout->get_ERTsm(i) << endl;
        }
    }
  return 0;
}

