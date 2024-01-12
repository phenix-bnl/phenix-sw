#include <PreviousEventReco.h>

#include <phool.h>
#include <PHCompositeNode.h>

#include <Fun4AllServer.h>

#include <PreviousEventv1.h>

#include <getClass.h>
#include <Event.h>

#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

PreviousEventReco::PreviousEventReco(const std::string &name): SubsysReco(name)
{
  return ;
}

int PreviousEventReco::Init(PHCompositeNode *topNode)
{
  int iret = CreateNodeTree(topNode);
  return iret;
}

int PreviousEventReco::CreateNodeTree(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << " DST Node is missing doing nothing" << endl;
      return -1;
    }
  PreviousEvent* prevevt = new PreviousEventv1();
  PHObjectNode_t *PreviousEventNode = new PHObjectNode_t(prevevt, "PreviousEvent", "PHObject"); // contain PHObject
  dstNode->addNode(PreviousEventNode);

  return 0;
}

int PreviousEventReco::process_event(PHCompositeNode *topNode)
{
  int iret = 0;

  PHNodeIterator iter(topNode);
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");

  if (!evt)
    {
      cout << PHWHERE << "0 Event Pointer" << endl;
      return -1;
    }

  PreviousEvent* prevevt = findNode::getClass<PreviousEvent>(topNode, "PreviousEvent");
  if (prevevt)
    {
     int eventnumber = evt->getEvtSequence();
     Packet *pkt14009 = evt->getPacket(14009);
     // the first 1,2,3 events are do not have the diff to the n-3/n-2/n-1 
     // event for obvious reasons. Those come a partition vector=0 which
     // triggers the warning message (which is from now on to be taken 
     // seriously)
     int imax = 4;
     if (eventnumber < 4)
       {
	 imax = eventnumber;
       }
      if (pkt14009)
        {
          int granaccpvec = pkt14009->iValue(0, "PARVECT") & 0x7FFFFFFF; // mask bit 31 which is for the shadow partition
          int n = 0;
          unsigned int clockcounter = static_cast <unsigned int> (pkt14009->iValue(0, "EVCLOCK")); // ivalue returns int

          for (int i = 1; i < imax; i++) // there are 4 events kept including the current one
            {
	      // the shadow partition enables us to not reset the gl1p for non physics events (ppg events)
	      // physics triggers have the partition vector 0x80000001, ppg triggers have 0x00000001
              if ((pkt14009->iValue(i, "PARVECT") & 0x7FFFFFFF )  == granaccpvec) // mask bit 31 for the shadow partition
                {
                  unsigned int thisclock = static_cast <unsigned int> (pkt14009->iValue(i, "EVCLOCK"));
                  if (verbosity > 0)
                    {
                      cout << "clockcounter: 0x" << hex << clockcounter
                           << ", thisclock: 0x" << thisclock << dec << endl;

                    }
                  if (thisclock < clockcounter)
                    {
                      prevevt->set_clockticks(clockcounter - thisclock, n);
                    }
                  else
                    {
                      if (verbosity > 0)
                        {

                          cout << "rollover: clockcounter: " << clockcounter
                               << ", thisclock: " << thisclock
                               << ",  0xFFFFFFFF - thisclock: " << 0xFFFFFFFF - thisclock
                               << ", result: " << clockcounter + (0xFFFFFFFF - thisclock) + 1
                               << endl;
                          cout << hex << "rollover: clockcounter: " << clockcounter
                               << ", thisclock: " << thisclock
                               << ",  0xFFFFFFFF - thisclock: " << 0xFFFFFFFF - thisclock
                               << ", result: " << clockcounter + (0xFFFFFFFF - thisclock) + 1
                               << dec << endl;
                        }
                      prevevt->set_clockticks(clockcounter + (0xFFFFFFFF - thisclock) + 1, n); // +1 to include the 0x0 clock counter
                    }
                  n++;
                }
              else
                {
                  cout << PHWHERE 
                       << " This should not happen, different gran vecs: " 
                       << granaccpvec << ", new: " 
                       << pkt14009->iValue(i, "PARVECT") 
                       << endl;
		  cout << "packet 14009 identify(): " << endl;
		  pkt14009->identify();
		  cout << "Event identify(): " << endl;
		  evt->identify();
                }
            }
          delete pkt14009;
        }
      if (verbosity > 0)
        {
          prevevt->identify();
        }
    }

  return iret;
}

