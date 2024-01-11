//INCLUDECHECKER: Removed this line: #include "CrkGetDCM.h"
#include "dCrkDCMWrapper.h"

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "Event.h"

//INCLUDECHECKER: Removed this line: #include "phool.h"

#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

static void reorder_headers(DCRKDCM_ST *dcm)
{
  // 6/8/2000 YA because of a documentation error, the header word
  // order of real DCM output and simulated DCM output is different.
  // I change the word order of real DCM to simulated one here.
  int detID = dcm->flag;
  int evno = dcm->module;
  int module = dcm->evno;
  int flag = dcm->clock;
  int clock = dcm->detid;
  dcm->flag = flag;
  dcm->module = module;
  dcm->evno = evno;
  dcm->clock = clock;
  dcm->detid = detID;
}

long
CrkGetDCM(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* dcmNode;
  PHNode *n1, *n2;
  TableNode_t *d;

  // Find the node with the Event object; it is called "PRDF".
  n1 = iter.findFirst("PHDataNode", "PRDF");
  if (!n1)
    {
      return 1;
    }

  // Find the DCM "directory" if it exists; otherwise, create it.
  n2 = iter.findFirst("PHCompositeNode", "DCM");
  if (!n2)
    {
      dcmNode = new PHCompositeNode("DCM");
      topNode->addNode(dcmNode);
    }
  else
    {
      dcmNode = static_cast<PHCompositeNode*>(n2);
    }

  // Extract the Event object from the node.
  EventNode_t* node = static_cast<EventNode_t*>(n1);
  Event* event = node->getData();
  if (!event)
    {
      return 1;
    }

  // Instantiate the DCM table for this subsystem, and attach it to
  // the DCM "directory" node.
  dCrkDCMWrapper* w;
  j = new PHNodeIterator(dcmNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode", "dCrkDCM"));
  if (!d)
    {
      w = new dCrkDCMWrapper("dCrkDCM", 1500);
      if (!w)
        {
          return 1;
        }
      d = new TableNode_t(w, "dCrkDCM");
      dcmNode->addNode(d);
    }
  else
    {
      w = static_cast<dCrkDCMWrapper*>(d->getData());
      if (!w)
        {
          return 1;
        }
    }
  delete j;

  DCRKDCM_ST* dCrkDCM = w->TableData();

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  int i = 0;
  static const int id_base = 6001;
  static const int max_packets = 40;
  static const int length = 498;
  int id, words;

  for (int k = 0; k < max_packets; k++)
    {
      id = id_base + k;

      if ((p = event->getPacket(id)) != 0)
        {
          // The packet size of DCM output was specified as 498 words in
          // DCM web page. Actually, the real size is 499 words because of
          // header word of 0x0FFFF.  To keep compatibility with old PRDF
          // file produced in VRDC(2000), we check the data lenght here by
          // getLength().  If data packets has the leading 0xFFFF, the
          // length is 505 If data packets does not have it, the length is
          // 504.  If the data length is 505, the first word (==0xFFFF) is
          // ignored

          int status;
          int pkt_length = p->getLength();
          if (pkt_length == 504)
            { 
	      // old MC PRDF
              status = p->fillIntArray(
                         (int *) & dCrkDCM[i].flag,   // address of buffer
                         length,                     // size of buffer
                         &words,                     // number of words read
                         "DATA");                   // omit header info
            }
          else if (pkt_length == 505)
            {
              // Real data or future MC PRDF. This means there is a leading
              // 0xFFFF as the first word of packet.  Since there are 3
              // words before flag member of dCrkDCM, we can transfer data
              // one word from "flag".
              int *address = (int*) & dCrkDCM[i].flag;
              --address;
              status = p->fillIntArray(
                         address,                    // address of buffer
                         length + 1,                   // size of buffer
                         &words,                     // number of words read
                         "DATA");                   // omit header info
              // Also change the order of header world so that the
              // definition of dCrkDCM table is unchagned.
              reorder_headers(dCrkDCM + i);
            }
          else
            { // wrong packet length. Data is corrupted.
              status = -2;
            }
          if (status != 0)
            {
              cout << "CrkGetDCM-W1  fillIntArray status = " << status << endl;
              delete p;
              return 1;
            }
          dCrkDCM[i].nWord = words;
          dCrkDCM[i].packetID = id;
          dCrkDCM[i].scheme = p->getHitFormat();

          i++;

          delete p;   // event->getPacket creates a packet object on the
          // heap, so delete it here!
        }
    }
  w->SetRowCount(i);

  return 0;
}
