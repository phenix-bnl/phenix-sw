//INCLUDECHECKER: Removed this line: #include "ZdcGetDCM.h"
#include "ZdcRaw.h"

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "Event.h"

//INCLUDECHECKER: Removed this line: #include "phool.h"
#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode <ZdcRaw> ZdcRawNode_t;

long
ZdcGetDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode);
  PHNode *n1;

  // Find the node with the Event object; it is called "PRDF".
  n1 = iter.findFirst("PHDataNode", "PRDF");
  if (!n1)    // perhaps it is a composite node?
    {
      n1 = iter.findFirst("PHCompositeNode", "PRDF");
    }

  if (!n1)
    {
      cout << "ZdcGetDCM-E5  failed to find PRDF node" << endl;
      return -1;
    }

  // Extract the Event object from the node.
  EventNode_t* node = dynamic_cast<EventNode_t*>(n1);
  Event* event = node->getData();
  if (!event)
    {
      cout << PHWHERE << "PRDF node is empty!" << endl;
      return -1;
    }

  ZdcRaw *zdcraw = NULL;
  PHTypedNodeIterator<ZdcRaw> zdcrawiter(topNode);
  ZdcRawNode_t *ZdcRawNode = zdcrawiter.find("ZdcRaw");
  if (ZdcRawNode)
    {
      zdcraw = ZdcRawNode->getData();
      if (!zdcraw)
        {
          cout << PHWHERE << " Unable to get ZdcRaw, is Node missing?" << endl;
          return -1;
        }
    }
  else
    {
      cout << PHWHERE << " ZdcRaw Node is missing" << endl;
      return -1;
    }

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  const int PacketTypes = 1;
  const int ZdcPacketId[PacketTypes] =
    {
      13001
    };

  const int numch_per_board = 8;

  Packet *pa = 0;
  int iret = -1;
  if ( (pa = event->getPacket(ZdcPacketId[0])) )
    {
      // we should probably do some quality checks here...
      //dcmbuf[index++] = pa->iValue(0, "ID");	// det id.
      //dcmbuf[index++] = pa->iValue(0, "EVTNR");	// event no.
      //dcmbuf[index++] = pa->iValue(0, "MODULE");	// module no.
      //dcmbuf[index++] = pa->iValue(0, "FLAG");	// flag word
      //dcmbuf[index++] = pa->iValue(0, "BCLK");	// beam clock
      //dcmbuf[index++] = pa->iValue(0, "BOARD");	// board id
      //dcmbuf[index++] = pa->iValue(0, "PARITY");	// parity word
      //dcmbuf[index++] = pa->iValue(0, "SUMMARY");	// summary word

      int nch = numch_per_board * pa->iValue(0, "NUM_BOARDS");
      for (int ch = 0; ch < nch; ch++)
        {
          zdcraw->AddZdcRawHit( pa->iValue(ch),
                                pa->iValue(ch, "T1"),
                                pa->iValue(ch, "T2"), ch );
        }

      delete pa;   // event->getPacket creates a packet object on the
      // heap, so delete it here!
      iret = 0;
    }

  return iret;
}

