#include "DumpPreviousEvent.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"

#include "PreviousEvent.h"

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

using namespace std;

typedef PHIODataNode<PreviousEvent> MyNode_t;

DumpPreviousEvent::DumpPreviousEvent(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPreviousEvent::process_Node(PHNode *myNode)
{
  PreviousEvent *previousevent = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      previousevent = thisNode->getData();
    }
  if (previousevent && previousevent->isValid())
    {
      for (int i = 0; i < 3; i++)
        {
          *fout << "previousevent->get_clockticks(" << i << "): " << previousevent->get_clockticks(i) << endl;
        }
    }
  return 0;
}

