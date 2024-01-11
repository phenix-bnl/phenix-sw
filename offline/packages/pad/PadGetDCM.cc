#include "PadGetDCM.h"
#include "dPadDCMWrapper.h"

#include "PHString.h"
#include "PHIODataNode.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "Event.h"

#include "phool.h"
#include <iostream>


using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

long
PadGetDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHNode *n1, *n2;
  TableNode_t *d;
  const char* tableName[3] = {"dPc1DCM","dPc2DCM","dPc3DCM"};

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

  // Instantiate the DCM tables for this subsystem, and attach them to
  // the DCM "directory" node.
  dPadDCMWrapper* w[3];
  j  = new PHNodeIterator(dcmNode);

  for (int n=0; n<3; n++) {
    d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode",tableName[n]));
    if (!d) {
      w[n] = new dPadDCMWrapper(tableName[n], 32);
      if (!w[n]) {
	return 1;
      }
      d = new TableNode_t(w[n],tableName[n]);
      dcmNode->addNode(d);
    }
    else {
      w[n] = static_cast<dPadDCMWrapper*>(d->getData());
      if (!w[n]) {
	return 1;
      }
    }
  }

  delete j;

  DPADDCM_ST* dPadDCM;

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  int pcnumber;
  static const int max_packets = 32;
  static const int length      = 132;

  for (pcnumber=0; pcnumber<3; pcnumber++) {

    int i       = 0;
    int id_base = 4001 + pcnumber*32;
    int id, words;
    Packet* p;

    dPadDCM = w[pcnumber]->TableData();

    for (int k = 0; k < max_packets; k++) {
      id = id_base + k;

      if ( (p = event->getPacket(id)) != 0) {

	int status
	  = p->fillIntArray(
			    (int *) &dPadDCM[i].Word[0], // address of buffer
			    length,                      // size of buffer
			    &words,                      // num of words read
			    "DATA");                     // omit header info
	if (status != 0) {
	  cout << "PadGetDCM-E1  fillIntArray status = " << status << endl;
	  delete p;
	  return 1;
	}

	i++;

	delete p;   // event->getPacket creates a packet object on the
                    // heap, so delete it here!
      }
      w[pcnumber]->SetRowCount(i);
    }

  }

  return 0;
}
