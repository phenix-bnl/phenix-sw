//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"
#include "PHPoint.h"

#include "VtxOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpVtxOut.h"

using namespace std;

typedef PHIODataNode<VtxOut> MyNode_t;

DumpVtxOut::DumpVtxOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int 
DumpVtxOut::process_Node(PHNode *myNode)
{
  VtxOut *vtxout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      vtxout = thisNode->getData();
    }
  if (vtxout)
    {
      *fout << "isValid(): " << vtxout->isValid() << endl;
      if (vtxout->isValid())
        {
          *fout << "get_ZVertex(): " << vtxout->get_ZVertex() << endl;
          *fout << "get_ZVertexError(): " << vtxout->get_ZVertexError() << endl;
          *fout << "isBbcVtx(): " << vtxout->isBbcVtx() << endl;
          if (vtxout->isBbcVtx())
            {
              *fout << "get_BbcVertex(): " << vtxout->get_BbcVertex() << endl;
              *fout << "get_BbcVertexError(): " << vtxout->get_BbcVertexError() << endl;
            }
          *fout << "isZdcVtx(): " << vtxout->isZdcVtx() << endl;
          if (vtxout->isZdcVtx())
            {
              *fout << "get_ZdcVertex(): " << vtxout->get_ZdcVertex() << endl;
              *fout << "get_ZdcVertexError(): " << vtxout->get_ZdcVertexError() << endl;
            }
          PHPoint vtx, vtxerr;
          vtx = vtxout->get_Vertex();
          vtxerr = vtxout->get_VertexError();
          *fout << "get_VertexX: " << vtx.getX() << endl;
          *fout << "get_VertexY: " << vtx.getY() << endl;
          *fout << "get_VertexZ: " << vtx.getZ() << endl;
          *fout << "get_VertexErrorX: " << vtxerr.getX() << endl;
          *fout << "get_VertexErrorY: " << vtxerr.getY() << endl;
          *fout << "get_VertexErrorZ: " << vtxerr.getZ() << endl;
        }
      vtxout->identify(*fout);
    }
  return 0;
}

