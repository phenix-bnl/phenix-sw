#include "DumpSvxQAInfo.h"

#include <SvxQAInfo.h>
#include <SvxParameters.h>

#include <PHIODataNode.h>

#include <string>

using namespace std;

typedef PHIODataNode<SvxQAInfo> MyNode_t;

DumpSvxQAInfo::DumpSvxQAInfo(const string &NodeName): 
  DumpObject(NodeName),
  node_written(0) // write runwise nodes only once
{
  write_run_event = 0; // do not write info for each event
  return ;
}

int DumpSvxQAInfo::process_Node(PHNode *myNode)
{
  SvxQAInfo *svxqainfo = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      svxqainfo = thisNode->getData();
    }
  if (!node_written && svxqainfo)
    {
      for (int i=0; i<SVXNMODULESTRIP; i++)
	{
	  *fout << "get_CellStuckEvent(" << i << "): " << svxqainfo->get_CellStuckEvent(i) << endl;
	}
       node_written = 1;
    }
  return 0;
}

