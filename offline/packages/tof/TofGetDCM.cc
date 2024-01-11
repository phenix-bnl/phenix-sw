//INCLUDECHECKER: Removed this line: #include "TofGetDCM.h"
#include "dTofDCMWrapper.h"

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
TofGetDCM(PHCompositeNode* topNode)
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
  dTofDCMWrapper* w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTofDCM"));
  if (!d) {
    w = new dTofDCMWrapper("dTofDCM", 8);
    if (!w) {
      return 1;
    }
    d = new TableNode_t(w,"dTofDCM");
    dcmNode->addNode(d);
  }
  else {
    w = static_cast<dTofDCMWrapper*>(d->getData());
    if (!w) {
      return 1;
    }
  }
  delete j;

  DTOFDCM_ST* dTofDCM   = w->TableData();

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  int i      = 0;
  static const int id_base     = 7001;
  static const int max_packets = 8;
  static const int length      = 1144;
  int id, words;

  for (int k = 0; k < max_packets; k++) {
    id = id_base + k;

    if ( (p = event->getPacket(id)) != 0) {

      int status
	= p->fillIntArray(
			  (int *) &dTofDCM[i].DCM[0],  // address of buffer
			  length,                      // size of buffer
			  &words,                      // number of words read
			  "DATA");                     // omit header info
      if (status != 0) {
	cout << "TofGetDCM-E1  fillIntArray status = " << status << endl;
	delete p;
	return 1;
      }

      dTofDCM[i].nWord    = words;
      dTofDCM[i].packetID = id;
      dTofDCM[i].scheme   = p->getHitFormat();

      delete p;   // event->getPacket creates a packet object on the
                  // heap, so delete it here!
      i++;
    }
  }
  w->SetRowCount(i);
  
  return 0;
}
