//INCLUDECHECKER: Removed this line: #include "EmcGetDCM.h"
#include "dEmcDCMDataWrapper.h"

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "Event.h"

//INCLUDECHECKER: Removed this line: #include "phool.h"

#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
EmcGetDCM(PHCompositeNode* topNode)
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
  dEmcDCMDataWrapper* w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcDCMData"));
  if (!d) {
    w = new dEmcDCMDataWrapper("dEmcDCMData", 500);
    if (!w) {
      return 1;
    }
    d = new TableNode_t(w,"dEmcDCMData");
    dcmNode->addNode(d);
  }
  else {
    w = static_cast<dEmcDCMDataWrapper*>(d->getData());
    if (!w) {
      return 1;
    }
  }
  delete j;

  DEMCDCMDATA_ST* dEmcDCMData   = w->TableData();

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  static const int id_base     = 8001;
  static const int max_packets = 192;
  static const int length      = 450;
  int  id, words;
  long i = 0;

  for (int k = 0; k < max_packets; k++) {
    id = id_base + k;

    if ( (p = event->getPacket(id)) != 0) {

      int status
	= p->fillIntArray(
			  (int *) &dEmcDCMData[i].DCM[0], // buffer address
			  length,                           // size of buffer
			  &words,                           // num words read
			  "DATA");                          // omit header

      if (status != 0) {
	cout << "EmcGetDCM-E1  fillIntArray status = " << status << endl;
	delete p;
	return 1;
      }

      dEmcDCMData[i].nWords   = words;
      dEmcDCMData[i].packetID = id;
      dEmcDCMData[i].scheme   = p->getHitFormat();

      delete p;   // event->getPacket creates a packet object on the
                  // heap, so delete it here!
      i++;
    }
  }
  w->SetRowCount(i);

  return 0;
}
