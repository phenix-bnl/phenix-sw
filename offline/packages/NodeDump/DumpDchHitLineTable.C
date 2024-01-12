//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "DchHitLineTable.hh"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpDchHitLineTable.h"

using namespace std;

typedef PHIODataNode<DchHitLineTable> MyNode_t;

DumpDchHitLineTable::DumpDchHitLineTable(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpDchHitLineTable::process_Node(PHNode *myNode)
{
  DchHitLineTable *dchhitlinetable = NULL;
  PHPoint dchpt;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      dchhitlinetable = thisNode->getData();
    }
  if (dchhitlinetable && dchhitlinetable->isValid())
    {

      *fout << "NHits: " << dchhitlinetable->Entries() << endl;
      *fout << "dchhitlinetable->getStatus(): " << dchhitlinetable->getStatus() << endl;
      for (int i = 0; i < dchhitlinetable->Entries(); i++)
        {
          *fout << "dchhitlinetable->getId(" << i << "): " << dchhitlinetable->getId(i) << endl;
          *fout << "dchhitlinetable->getIdmirror(" << i << "): " << dchhitlinetable->getIdmirror(i) << endl;
          *fout << "dchhitlinetable->getArm(" << i << "): " << dchhitlinetable->getArm(i) << endl;
          *fout << "dchhitlinetable->getPlane(" << i << "): " << dchhitlinetable->getPlane(i) << endl;
          *fout << "dchhitlinetable->getCell(" << i << "): " << dchhitlinetable->getCell(i) << endl;
          *fout << "dchhitlinetable->getSide(" << i << "): " << dchhitlinetable->getSide(i) << endl;
          *fout << "dchhitlinetable->getTime1(" << i << "): " << dchhitlinetable->getTime1(i) << endl;
          *fout << "dchhitlinetable->getWidth(" << i << "): " << dchhitlinetable->getWidth(i) << endl;
          dchpt = dchhitlinetable->getXYZ(i);
          *fout << "dchhitlinetable->getX(" << i << "): " << dchpt.getX() << endl;
          *fout << "dchhitlinetable->getY(" << i << "): " << dchpt.getY() << endl;
          *fout << "dchhitlinetable->getZ(" << i << "): " << dchpt.getZ() << endl;
        }
    }
  return 0;
}

