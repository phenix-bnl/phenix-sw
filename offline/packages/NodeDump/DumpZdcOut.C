//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

//INCLUDECHECKER: Removed this line: #include "Zdc.hh"
#include "ZdcOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpZdcOut.h"

using namespace std;

typedef PHIODataNode<ZdcOut> MyNode_t;

DumpZdcOut::DumpZdcOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int 
DumpZdcOut::process_Node(PHNode *myNode)
{
  ZdcOut *zdcout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      zdcout = thisNode->getData();
    }
  if (zdcout)
    {
      *fout << "ZdcOut->isValid(): " <<  zdcout->isValid() << endl;
      if (zdcout->isValid())
        {

          *fout << "get_Zvertex: " << zdcout->get_Zvertex() << endl;
          *fout << "get_ZvertexError: " << zdcout->get_ZvertexError() << endl;
          *fout << "get_TimeZero: " << zdcout->get_TimeZero() << endl;
          *fout << "get_TimeZeroError: " << zdcout->get_TimeZeroError() << endl;
          *fout << "get_Energy(Zdc::North): " << zdcout->get_Energy(Zdc::North) << endl;
          *fout << "get_Energy(Zdc::South): " << zdcout->get_Energy(Zdc::South) << endl;
          *fout << "get_Timing(Zdc::North): " << zdcout->get_Timing(Zdc::North) << endl;
          *fout << "get_Timing(Zdc::South): " << zdcout->get_Timing(Zdc::South) << endl;
          for (short i = 0;i < 8;i++)
            {
              *fout << ": get_Charge(" << i << "): " << zdcout->get_Charge(i) << endl;
              *fout << ": get_Time0(" << i << "): " << zdcout->get_Time0(i) << endl;
              *fout << ": get_Time1(" << i << "): " << zdcout->get_Time1(i) << endl;
            }
        }

    }
  return 0;
}

