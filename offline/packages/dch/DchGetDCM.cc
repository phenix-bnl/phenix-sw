//INCLUDECHECKER: Removed this line: #include <iostream>
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "Event.h"
#include "dDchDCMWrapper.h"
//INCLUDECHECKER: Removed this line: #include "DchGetDCM.h"

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
DchGetDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHNode *n1,*n2;
  TableNode_t *d;

  // Find the node with the Event object; it is called "PRDF".
  n1 = iter.findFirst("PHDataNode", "PRDF");
  if (!n1) {
    return 1;
  }

  // Find the DCM "directory" if it exists; otherwise, create it.
  n2 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n2) {
    dcmNode = new PHCompositeNode("DCM");
    topNode->addNode(dcmNode);
  }
  else {
    dcmNode = static_cast<PHCompositeNode*>(n2);
  }

  // Extract the Event object from the node.
  EventNode_t* node = static_cast<EventNode_t*>(n1);
  Event* event = node->getData();
  if (!event) {
    return 1;
  }

  // Instantiate the DCM table for this subsystem, and attach it to
  // the DCM "directory" node.
  dDchDCMWrapper* w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dDchDCM"));
  if (!d) {
    w = new dDchDCMWrapper("dDchDCM", 160);
    if (!w) {
      return 1;
    }
    d = new TableNode_t(w,"dDchDCM");
    dcmNode->addNode(d);
  }
  else {
    w = static_cast<dDchDCMWrapper*>(d->getData());
    if (!w) {
      return 1;
    }
  }
  delete j;

  DDCHDCM_ST* dDchDCM   = w->TableData();

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  int i      = 0;
  int id     = 3001;
  int length = 971;
  int words;

  for (id=3001; id <= 3160; id++)  {

    p = event->getPacket(id);

    if (p)
      {

	p->fillIntArray(
			(int *) &dDchDCM[i].DCM[0],  // address of buffer
			length,                      // size of buffer
			&words,                      // number of words read
			"DATA");                     // omit header info

	dDchDCM[i].nWord    = words;
	dDchDCM[i].packetID = id;
	dDchDCM[i].scheme   = p->getHitFormat();

	delete p;   // event->getPacket creates a packet object on the
	            // heap, so delete it here!
	i++;
      }
  }
  w->SetRowCount(i);

  return 0;
}