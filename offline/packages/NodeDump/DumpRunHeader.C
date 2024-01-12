#include <string>

#include <PHIODataNode.h>

#include <RunHeader.h>

#include <DumpRunHeader.h>

using namespace std;

typedef PHIODataNode<RunHeader> MyNode_t;

DumpRunHeader::DumpRunHeader(const string &NodeName): DumpObject(NodeName)
{
  write_run_event = 0; // do not write info for each event
  node_written = 0; // write runwise nodes only once
  return ;
}

int DumpRunHeader::process_Node(PHNode *myNode)
{
  
  RunHeader *runheader = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      runheader = thisNode->getData();
    }
  if (!node_written && runheader)
    {
      *fout << "RunHeader->isValid(): " << runheader->isValid() << endl;
      if (runheader->isValid())
        {
          *fout << "get_RunNumber(): " << runheader->get_RunNumber() << endl;
          *fout << "get_TimeStart(): " << (int) runheader->get_TimeStart() << endl;
          *fout << "get_TimeStop(): " << (int) runheader->get_TimeStop() << endl;
          *fout << "get_currentNorth(): " << runheader->get_currentNorth() << endl;
          *fout << "get_currentSouth(): " << runheader->get_currentSouth() << endl;
          *fout << "get_currentCentral(): " << runheader->get_currentCentral() << endl;
          node_written = 1;
        }
    }
  return 0;
}
