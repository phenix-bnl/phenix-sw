#include "phool.h"
#include "PHCompositeNode.h"
#include "Event.h"
#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"

#include "mTecUnpackModule.h"
#include "TecOutV1.hh"
#include "TecTrack.hh"
#include "TecHit.hh"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode<TecOutV1> TecOutNode_t;

mTecUnpackModule::mTecUnpackModule()
{
  Verbose = 0;
  FadcCut = 2;
}

PHBoolean 
mTecUnpackModule::event(PHCompositeNode *topNode)
{
  return False;
}

PHBoolean 
mTecUnpackModule::event(PHCompositeNode *topNode,
			TecAddressObject* TAO)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode* tecNode;
  PHNode *n1;

  // Find the node with the Event object; it is called "PRDF".
  n1 = iter.findFirst("PHDataNode", "PRDF");
  if (!n1)
    {
      return False;
    }

  // Extract the Event object from the node.
  EventNode_t* node = static_cast<EventNode_t*>(n1);
  Event* event = node->getData();
  if (!event)
    {
      return False;
    }

  // Try to find TecOut in the node tree
  PHTypedNodeIterator<TecOutV1> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;

  if (TecOutNode)
    {
      tecout = (TecOutV1*)TecOutNode->getData();
    }
  else
    {	
      // TecOut does not exist. Add it to node tree.
      tecNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode",
							     "TEC"));
      if (!tecNode)
        {
          return False;
        }
      tecout = new TecOutV1();
      PHIODataNode<PHObject>* TecOutNodeNew = 
	new PHIODataNode<PHObject>(tecout, "TecOutV1", "PHObject");
      tecNode->addNode(TecOutNodeNew);
    }

  // Set run number
  tecout->setRunNumber(event->getRunNumber());
  
  // Loop over all TEC packets for this event and fill TecOutV1 object
  Packet* p = 0;
  struct tecChannelList
  {
    int channel;
    int time;
    int value;
  }
  tl[10000];

  for (int i = 0; i < TECMAXPACKETS; i++)
    {
      int id = TECBASEPACKETID + i;

      if ((p = event->getPacket(id)) != 0)
        {
          // Get hardware indices from TecAddressObject using packet ID
          int crate = TAO->getCrate(id);
          int slot = TAO->getSlot(id);
          int psadd1 = TAO->getPsadd1(id);
          int psadd2 = TAO->getPsadd2(id);

          if (crate >= 0 && slot >= 0)
            {
              int nw = 0;
              int count = 
		p->fillIntArray((int *) tl,
				sizeof(struct tecChannelList) * 10000,
				&nw, "SPARSE");
              for (int j = 0; j < count; j++)
                {
		  // Loop over non-zero channels in this packet
                  int channel = tl[j].channel;
                  int psadd = -1;
                  if ((channel < 32) && ((psadd1 != 15) || (psadd1 != -1)))
                    {
                      psadd = psadd1;
                    }
                  else
                    {
                      if ((channel >= 32) && (psadd2 != 15 || psadd1 != -1))
                        {
                          psadd = psadd2;
                        }
                      else
                        {   // this should never happen
                          cerr << __FILE__ << ":" << __LINE__ << " "
			       << "Hard/Soft Map ERROR: Wrong psadd: " 
			       << id << " "
			       << crate << " " << slot << " "
			       << psadd1 << " " << psadd2 << endl;
                        }
                    }

                  int index = -1, sector = -1, side = -1;
		  int plane = -1, wire = -1, bin, adc;
                  PHBoolean status = TAO->HardToSoft(crate, slot, channel, 
						     sector, side, plane, 
						     wire);
                  if (status)
                    {
                      index = sector * TECMAXPLANE * TECMAXSIDE 
			+ plane * TECMAXSIDE + side;
                    }
                  else
                    {
		      // This should never happen
                      cerr << __FILE__ << ":" << __LINE__ << " "
			   << "Hard/Soft Map ERROR: No software indices: "
			   << crate
			   << " " << slot << " " << psadd << " " 
			   << tl[j].channel << endl;
                    }
                  if (psadd > -1 && status)
                    {    
		      // No decoding problems
                      float xyz[3] = {0.0, 0.0, 0.0};
                      float charge = 0.0;
                      int trackid = -1;
                      bin = tl[j].time;
                      adc = tl[j].value;
                      if (adc > FadcCut)
                        {
			  tecout->AddTecHit(index, wire, bin, 
					    adc, charge,
					    xyz, trackid);
			}
		    }
                } 
            } 
          else
            {
              cout << "crate,slot: " << crate << " " << slot << endl;
            }
          delete p;
        }
    }

  return True;
}

void 
mTecUnpackModule::set_FadcCutFromTimeStamp(PHTimeStamp* ts)
{
  // This is beginning of run 5 for Tec
  PHTimeStamp run5begins = PHTimeStamp(2004, 10, 1, 0, 0, 0); 
  if (*ts > run5begins)
    {
      FadcCut = 1;
    }
  else
    {
      FadcCut = 2;
    }
  return;
}



