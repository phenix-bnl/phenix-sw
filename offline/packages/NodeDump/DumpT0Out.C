//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "T0Out.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpT0Out.h"

using namespace std;

typedef PHIODataNode<T0Out> MyNode_t;

DumpT0Out::DumpT0Out(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpT0Out::process_Node(PHNode *myNode)
{
  T0Out *t0out = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      t0out = thisNode->getData();
    }
  if (t0out && t0out->isValid())
    {
      *fout << "t0out->isValid((): " << t0out->isValid() << endl;
      *fout << "T0List(): " << t0out->get_T0List() << endl;
      if (t0out->isValid())
        {
          *fout << "get_T0(): " << t0out->get_T0() << endl;
          *fout << "get_T0Error(): " << t0out->get_T0Error() << endl;
          *fout << "isBbcT0(): " << t0out->isBbcT0() << endl;
          if (t0out->isBbcT0())
            {
              *fout << "get_BbcT0(): " << t0out->get_BbcT0() << endl;
            }
          *fout << "isZdcT0(): " << t0out->isZdcT0() << endl;
          if (t0out->isZdcT0())
            {
              *fout << "get_ZdcT0(): " << t0out->get_ZdcT0() << endl;
            }
          *fout << "isTzrT0(): " << t0out->isTzrT0() << endl;
          if (t0out->isTzrT0())
            {
              *fout << "get_TzrT0(): " << t0out->get_TzrT0() << endl;
            }
          *fout << "isFkeT0(): " << t0out->isFkeT0() << endl;
          if (t0out->isFkeT0())
            {
              *fout << "get_FkeT0(): " << t0out->get_FkeT0() << endl;
            }
          *fout << "isNtcT0(): " << t0out->isNtcT0() << endl;
          if (t0out->isNtcT0())
            {
              *fout << "get_NtcT0(): " << t0out->get_NtcT0() << endl;
            }
	}
    }
  return 0;
}

