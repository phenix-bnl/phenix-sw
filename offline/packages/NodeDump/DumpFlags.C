#include <DumpFlags.h>
#include <FlagSave.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<FlagSave> MyNode_t;

DumpFlags::DumpFlags(const string &NodeName): DumpObject(NodeName)
{
  write_run_event = 0; // do not write info for each event
  node_written = 0; // write flagswise nodes only once
  return ;
}

int DumpFlags::process_Node(PHNode *myNode)
{
  
  FlagSave *flags = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      flags = thisNode->getData();
    }
  if (!node_written && flags)
    {
      *fout << "Flags->isValid(): " << flags->isValid() << endl;
      if (flags->isValid())
        {
	  flags->identify(*fout);
          node_written = 1;
        }
    }
  return 0;
}

