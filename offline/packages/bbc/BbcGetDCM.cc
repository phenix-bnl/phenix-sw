//INCLUDECHECKER: Removed this line: #include "BbcGetDCM.h"
#include "dBbcDCMWrapper.h"

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
BbcGetDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHNode *n1,*n2;
  TableNode_t *d;

  // Find the node with the Event object; it is called "PRDF".
  n1 = iter.findFirst("PHDataNode", "PRDF");
  if (!n1) {
    cout << "BbcGetDCM-E5  failed to find PRDF node" << endl;
    return 1;
  }

  // Extract the Event object from the node.
  EventNode_t* node = static_cast<EventNode_t*>(n1);
  Event* event = node->getData();
  if (!event) {
    cout << PHWHERE << "BbcGetDCM-E5  PRDF node is empty!" << endl;
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

  // Instantiate the DCM table for this subsystem, and attach it to
  // the DCM "directory" node.
  dBbcDCMWrapper* w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dBbcDCM"));
  if (!d) {
    w = new dBbcDCMWrapper("dBbcDCM", 1);
    if (!w) {
      cout << "BbcGetDCM-E1  failed to create new dBbcDCMWrapper" << endl;
      return 1;
    }
    d = new TableNode_t(w,"dBbcDCM");
    dcmNode->addNode(d);
  }
  else {
    w = static_cast<dBbcDCMWrapper*>(d->getData());
    if (!w) {
      cout << "BbcGetDCM-E2  no dBbcDCMWrapper in data node" << endl;
      return 1;
    }
  }
  delete j;

  DBBCDCM_ST*    dBbcDCM   = w->TableData();

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  int i      = 0;
  static const int id_base     = 1001;
  static const int max_packets = 1;
  static const int length      = 408;
  int id, words;

  for (int k = 0; k < max_packets; k++) {
    id = id_base + k;

    if ( (p = event->getPacket(id)) != 0) {

      int status
   	= p->fillIntArray(
   			  (int *) &dBbcDCM[i].DCM[0],  // address of buffer
   			  length,                      // size of buffer
   			  &words,                      // number of words read
   			  "DATA");                     // omit header info
      if (status != 0) {
 	cout << "BbcGetDCM-W1  fillIntArray status = " << status << endl;
 	delete p;
 	return 1;
      }

      dBbcDCM[i].nWord    = words;
      dBbcDCM[i].packetID = id;
      dBbcDCM[i].scheme   = p->getHitFormat();

      delete p;   // event->getPacket creates a packet object on the
                  // heap, so delete it here!
      i++;
    }
  }
  w->SetRowCount(i);

  return 0;
}
