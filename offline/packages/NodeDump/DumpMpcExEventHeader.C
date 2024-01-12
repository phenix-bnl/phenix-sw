#include "DumpMpcExEventHeader.h"

#include <MpcExEventHeader.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<MpcExEventHeader> MyNode_t;

DumpMpcExEventHeader::DumpMpcExEventHeader(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMpcExEventHeader::process_Node(PHNode *myNode)
{
  MpcExEventHeader *mpcexeventheader = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mpcexeventheader = thisNode->getData();
    }
  if (mpcexeventheader)
    {
      *fout << "getStack(): " << mpcexeventheader->getStack() << endl;
      *fout << "getStatephaseSize(): " << mpcexeventheader->getStatephaseSize() << endl;
      for (unsigned int i=0; i<mpcexeventheader->getStatephaseSize();i++)
	{
	  *fout << "getStatephase(" << i << "): " << mpcexeventheader->getStatephase(i) << endl;
	}

      *fout << "getCellIDsSize(): " << mpcexeventheader->getCellIDsSize() << endl;
      for (unsigned int i=0; i<mpcexeventheader->getCellIDsSize();i++)
	{
	  *fout << "getCellIDs(" << i << "): " << mpcexeventheader->getCellIDs(i) << endl;
	}
    }
  return 0;
}

