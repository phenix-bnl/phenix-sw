#include "TecGetDCM.h"
#include "dTecDcmDataWrapper.h"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "Event.h"

#include <iostream>
#include "phool.h"

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
TecGetDCM(PHCompositeNode* topNode)
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
  dTecDcmDataWrapper* w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dTecDcmData"));
  if (!d) {
    w = new dTecDcmDataWrapper("dTecDcmData", 8000);
    if (!w) {
      return 1;
    }
    d = new TableNode_t(w,"dTecDcmData");
    dcmNode->addNode(d);
  }
  else {
    w = static_cast<dTecDcmDataWrapper*>(d->getData());
    if (!w) {
      return 1;
    }
  }
  delete j;

  DTECDCMDATA_ST* dTecDcmData   = w->TableData();

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  int i      = 0;
  static const int id_base     = 5001;
  static const int max_packets = 800;
  static const int length      = 1352;
  int id, words;

  for (int k = 0; k < max_packets; k++) {
    id = id_base + k;

    if ( (p = event->getPacket(id)) != 0) {

      int status
	= p->fillIntArray(
			  (int *) &dTecDcmData[i].DCM[0],  // address of buffer
			  length,                      // size of buffer
			  &words,                      // number of words read
			  "DATA");                     // omit header info
      if (status != 0) {
	cout << "TecGetDCM-E1  fillIntArray status = " << status << endl;
	delete p;
	return 1;
      }

      dTecDcmData[i].Nwords   = words;
      dTecDcmData[i].packetID = id;
      dTecDcmData[i].scheme   = p->getHitFormat();

      delete p;   // event->getPacket creates a packet object on the
                  // heap, so delete it here!
      i++;
    }
  }
  w->SetRowCount(i);

  return 0;
}
