//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "Bbc.hh"
#include "BbcOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpBbcOut.h"

using namespace std;

typedef PHIODataNode<BbcOut> MyNode_t;

DumpBbcOut::DumpBbcOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpBbcOut::process_Node(PHNode *myNode)
{
  BbcOut *bbcout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      bbcout = thisNode->getData();
    }
  if (bbcout && bbcout->isValid())
    {
      *fout << "BbcOut->get_VertexPoint: " << bbcout->get_VertexPoint() << endl;
      *fout << "BbcOut->get_dVertexPoint: " << bbcout->get_dVertexPoint() << endl;
      *fout << "BbcOut->get_TimeZero: " << bbcout->get_TimeZero() << endl;
      *fout << "BbcOut->get_dTimeZero: " << bbcout->get_dTimeZero() << endl;

      *fout << "BbcOut->get_nPmt(Bbc::North): " << bbcout->get_nPmt(Bbc::North) << endl;
      *fout << "BbcOut->get_nPmt(Bbc::South): " << bbcout->get_nPmt(Bbc::South) << endl;
      *fout << "BbcOut->get_ChargeSum(Bbc::North): " << bbcout->get_ChargeSum(Bbc::North) << endl;
      *fout << "BbcOut->get_ChargeSum(Bbc::South): " << bbcout->get_ChargeSum(Bbc::South) << endl;
      *fout << "BbcOut->get_Timing(Bbc::North): " << bbcout->get_Timing(Bbc::North) << endl;
      *fout << "BbcOut->get_Timing(Bbc::South): " << bbcout->get_Timing(Bbc::South) << endl;
    }
  return 0;
}

