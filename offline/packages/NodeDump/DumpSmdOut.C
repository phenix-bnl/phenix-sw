//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "SmdOut.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpSmdOut.h"

using namespace std;

typedef PHIODataNode<SmdOut> MyNode_t;

DumpSmdOut::DumpSmdOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpSmdOut::process_Node(PHNode *myNode)
{
  SmdOut *smdout = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      smdout = thisNode->getData();
    }
  if (smdout)
    {
      *fout << "SmdOut->isValid(): " <<  smdout->isValid() << endl;
      if (smdout->isValid())
        {

          for (short i = 0;i < 32;i++)
            {
              *fout << ": get_Charge(" << i << "): " << smdout->get_Charge(i) << endl;
              *fout << ": get_Time0(" << i << "): " << smdout->get_Time0(i) << endl;
              *fout << ": get_Time1(" << i << "): " << smdout->get_Time1(i) << endl;
            }
	  for (short i = 0; i< 2; i++)
	    {
              *fout << ": get_Xpos(" << i << "): " << smdout->get_Xpos(i) << endl;
              *fout << ": get_Ypos(" << i << "): " << smdout->get_Ypos(i) << endl;
              *fout << ": get_Energy(" << i << "): " << smdout->get_Energy(i) << endl;
	    }
        }

    }
  return 0;
}

