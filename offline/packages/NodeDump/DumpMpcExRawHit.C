#include "DumpMpcExRawHit.h"

#include "MpcExRawHit.h"

#include "PHIODataNode.h"

#include <string>

using namespace std;

typedef PHIODataNode<MpcExRawHit> MyNode_t;

DumpMpcExRawHit::DumpMpcExRawHit(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMpcExRawHit::process_Node(PHNode *myNode)
{
  MpcExRawHit *mpcexraw = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mpcexraw = thisNode->getData();
    }
  if (mpcexraw)
    {
      *fout << "getnhits(): " << mpcexraw->getnhits() << endl;
      for (unsigned int i = 0; i < mpcexraw->getnhits(); i++)
        {
          *fout << "Hit # " << i << ": val: " << hex << mpcexraw->gethit(i) << dec  << endl;
        }
    }
  return 0;
}

