//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "EventHeader.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpEventHeader.h"

using namespace std;

typedef PHIODataNode<EventHeader> MyNode_t;

DumpEventHeader::DumpEventHeader(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpEventHeader::process_Node(PHNode *myNode)
{
  EventHeader *eventheader = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      eventheader = thisNode->getData();
    }
  if (eventheader)
    {
      *fout << "EventHeader->isValid: " << eventheader->isValid() << endl;
      if (eventheader->isValid())
        {
          *fout << "get_EvtSequence(): " << eventheader->get_EvtSequence() << endl;
          *fout << "get_EvtType(): " << eventheader->get_EvtType() << endl;
          *fout << "get_TimeStamp(): " << (int) eventheader->get_TimeStamp() << endl;
	  if (verbosity > 0)
	    {
	      eventheader->identify();
	    }          
        }
    }
  return 0;
}

