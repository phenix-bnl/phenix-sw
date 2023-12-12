#include "CheckHbd.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <packet.h>

using namespace std;

CheckHbd::CheckHbd():
  GranuleCheck("HBD")
{
  GranuleNumber = GRAN_NO_HBD;
  return ;
}

int 
CheckHbd::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin = 22001;
  unsigned int ipktmax = 22012;
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  return 0;
}

int CheckHbd::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

unsigned int
CheckHbd::LocalEventCounter()
{
  return p->iValue(0, "TRIGGER");
}

int
CheckHbd::FEMEventSequenceCheck()
{
  int local_event_counter = LocalEventCounter();
  if (! rawdatacheck->GL1exists())
    {
      if (local_event_counter != rawdatacheck->EventNumber() + LocalEventNumberOffset())
        {
          // 	    cout << "packetiter->PacketId: " << packetiter->PacketId
          // 	    << "local_event_counter: " << local_event_counter
          // 	    << "rawdatacheck->EventNumber() + LocalEventNumberOffset(): "
          // 	    << rawdatacheck->EventNumber() + LocalEventNumberOffset() << endl;
          packetiter->FEMEvtSeqError++;
          return -1;
        }
    }
  else
    {
      if (local_event_counter != ((int) GlobalEventCounter() & 0xFFF))
        {
          // 	    cout << "packetiter->PacketId: " << packetiter->PacketId
          // 	    << "local_event_counter: " << local_event_counter
          // 	    << "GlobalEventCounter(): "
          // 	    << GlobalEventCounter() << endl;
          packetiter->FEMEvtSeqError++;
          return -1;
        }
    }
  return 0;
}
