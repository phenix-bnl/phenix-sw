#include <Fun4AllReturnCodes.h>
#include "mEmcPRDFToRawModule.h"
#include "emcNodeHelper.h"

#include "dEmcRawDataWrapper.h"
#include "PHNodeIterator.h"
#include "PHCompositeNode.h"
#include "PHDataNode.h"
#include "Event.h"

#include <iostream>
using namespace std;

static const int FeeToSoftwareMap[144] = 
{ 121, 120, 133, 132,  97,  96, 109, 108, 73, 72, 85, 84,
   49,  48,  61,  60,  25,  24,  37,  36,  1,  0, 13, 12,
  123, 122, 135, 134,  99,  98, 111, 110, 75, 74, 87, 86,
   51,  50,  63,  62,  27,  26,  39,  38,  3,  2, 15, 14,
  125, 124, 137, 136, 101, 100, 113, 112, 77, 76, 89, 88,
   53,  52,  65,  64,  29,  28,  41,  40,  5,  4, 17, 16,
  127, 126, 139, 138, 103, 102, 115, 114, 79, 78, 91, 90,
   55,  54,  67,  66,  31,  30,  43,  42,  7,  6, 19, 18,
  129, 128, 141, 140, 105, 104, 117, 116, 81, 80, 93, 92,
   57,  56,  69,  68,  33,  32,  45,  44,  9,  8, 21, 20,
  131, 130, 143, 142, 107, 106, 119, 118, 83, 82, 95, 94,
   59,  58,  71,  70,  35,  34,  47,  46, 11, 10, 23, 22
};

//_____________________________________________________________________________
mEmcPRDFToRawModule::mEmcPRDFToRawModule(): SubsysReco("mEmcPRDFToRawModule")
{
}

int
mEmcPRDFToRawModule::process_event(PHCompositeNode* topNode)
{
  PHCompositeNode* emcNode = emcNodeHelper::findCompositeNode(topNode,"EMC");

  if (!emcNode)
    {
      cerr << __FILE__ << ":" << __LINE__ << " Could not find EMC node !"
	   << endl;
      return ABORTRUN;
    }

  dEmcRawDataWrapper* demcraw = 
    emcNodeHelper::getTable<dEmcRawDataWrapper>("dEmcRawData",emcNode);

  if (!demcraw)
    {
      cerr << __FILE__ << ":" << __LINE__ << " Could not find dEmcRawData table !"
	   << endl;
      return ABORTRUN;
    }

  PHNodeIterator iter(topNode);
  PHNode* n1 = iter.findFirst("PHDataNode", "PRDF");
  if ( !n1 )
    {
      cerr << __FILE__ << ":" << __LINE__ << " Could not find PRDF node ! "
	   << endl;
      return ABORTRUN;
    }

  PHDataNode<Event>* eventNode = static_cast<PHDataNode<Event>*>(n1);
  Event* event = eventNode->getData();
  if (!event)
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Could not find Event in PRDF node ! "
	   << endl;
      return ABORTRUN;
    }

  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet* p;
  static const int id_base     = 8000;
  static const int max_packets = 172;
  short id = 0;	// unique identifier of the RAW entry

  for (int ipkt = 1; ipkt <= max_packets; ipkt++)
    {
      int pktid = id_base + ipkt;

      // get packet
      p = event->getPacket(pktid);
      if ( p != 0 )
        {
          // go through all channels, save those that have a hit
          for (int ifemch=0; ifemch<144; ifemch++)
            {
              // check if there is a hit (ie, if all data values are 0)
              if ( (p->iValue(ifemch, 0) + p->iValue(ifemch, 0) + p->iValue(ifemch, 0) +
                    p->iValue(ifemch, 0) + p->iValue(ifemch, 0)) == 0 )
                {
                  continue;
                }

              // now get the swkey and detector type (hwkey is not used?)
              short detector_type = -1;
              int arm, sector, iy, iz;
              int swkey = -1;
              int module = p->iValue(0, "MODULE");
              if (module != ipkt)
                {
                  cout << "mEmcPRDFToRaw: error, FEM and packetid mismatch" << endl;
                }

              // module number from packet should always match packetid-8000, but if not
              // we use the packetid as the correct identity
              if (ipkt <= 72)
                {
                  arm = 0;
                  sector = (ipkt-1)/18;
                }
              else
                {
                  arm = 1;
                  if (ipkt <= 108)
                    {
                      sector = (ipkt-73)/18 + 2;
                      /* iy = ((ipkt-(sector*18)-1)/6)*12 + FeeToSoftwareMap[ifemch]/12; -- flagged by scan-build as dead assignment */
                      /* iz = ((ipkt-(sector*18)-1)%6)*12 + FeeToSoftwareMap[ifemch]%12; -- flagged by scan-build as dead assignment */
                    }
                  else             sector = (ipkt-109)/32;
                }

              if (ipkt <= 108)
                {
                  iy = ( ((ipkt-1)%18)/6 )*12 + FeeToSoftwareMap[ifemch]/12;
                  iz = ((ipkt - 1)%6)*12 + FeeToSoftwareMap[ifemch]%12;
                }
              else
                {
                  iy = ( ((ipkt-109)%32)/8 )*12 + FeeToSoftwareMap[ifemch]/12;
                  iz = ((ipkt - 109)%8)*12 + FeeToSoftwareMap[ifemch]%12;
                }

              swkey = arm*100000 + sector*10000 + iy*100 + iz;

              if (ipkt < 109) detector_type = 1;	// PbSc
              else            detector_type = 2;	// PbGl

              demcraw->set_id(id, id+1);	// starts counting at 1
              demcraw->set_evno(id, p->iValue(0,"EVTNR") );
              demcraw->set_hwkey(id, 0);
              demcraw->set_swkey(id, swkey);
              demcraw->set_type(id, detector_type);
              demcraw->set_adclopre(id,  p->iValue(ifemch, 3) ^ 0xfff );
              demcraw->set_adclopost(id, p->iValue(ifemch, 1) ^ 0xfff );
              demcraw->set_adchipre(id,  p->iValue(ifemch, 4) ^ 0xfff );
              demcraw->set_adchipost(id, p->iValue(ifemch, 2) ^ 0xfff );
              demcraw->set_tdc(id,       p->iValue(ifemch, 0) ^ 0xfff );

              id++;
            }

          delete p;	// free the sucker
        }

    }

  demcraw->SetRowCount( id );

  return EVENT_OK;
}
