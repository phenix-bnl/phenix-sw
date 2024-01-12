#include "DumpLvl2OutArray.h"

#include <PHIODataNode.h>
#include <Lvl2OutArray.h>

#include <string>

using namespace std;

typedef PHIODataNode<Lvl2OutArray> MyNode_t;

DumpLvl2OutArray::DumpLvl2OutArray(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int 
DumpLvl2OutArray::process_Node(PHNode *myNode)
{
  Lvl2OutArray *lvl2outarray = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      lvl2outarray = thisNode->getData();
    }
  if (lvl2outarray)
    {
      *fout << "isValid(): " << lvl2outarray->isValid() << endl;
      if (lvl2outarray->isValid())
        {
	  int npart = lvl2outarray->get_npart();
          *fout << "get_npart(): " << npart << endl;
	  for (int i=0;i<npart;i++)
	    {
	      *fout << "getname(" << i << "): " << lvl2outarray->getname(i) << endl;
	      *fout << "getversion(" << i << "): " << lvl2outarray->getversion(i) << endl;
	      *fout << "getendianism(" << i << "): " << lvl2outarray->getendianism(i) << endl;
	      unsigned int dlen = lvl2outarray->getdatalength(i);
	      *fout << "getdatalength(" << i << "): " << dlen << endl;
              PHDWORD *dw = lvl2outarray->getdata(i);
	      for(unsigned int j = 0; j< dlen ;j++)
		{
		  *fout << "getdata(" << i << ")[" << j << "]: " << *dw++ << endl;
		}
	    }
        }
    }
  return 0;
}
