#include "CheckMpc.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <packet.h>

using namespace std;

CheckMpc::CheckMpc(const string &name):
  GranuleCheck(name)
{
  GranuleNumber = GRAN_NO_FCAL;
  return ;
}

int 
CheckMpc::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin = 21101;
  unsigned int ipktmax = 21106;
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  return 0;
}

int 
CheckMpc::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

unsigned int
CheckMpc::LocalEventCounter()
{
  return p->iValue(0, "TRIGGER");
}

int
CheckMpc::GlinkCheck()
{
  packetiter->GlinkError = -1;
  return 0;
}

int
CheckMpc::FEMEventSequenceCheck()
{
  if (! p->iValue(0, "NRMODULES")) // check for empty packet
    {
      return 0;
    }
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

int 
CheckMpc::FEMClockCounterCheck()
{
  if (! p->iValue(0, "NRMODULES")) // check for empty packet
    {
      return 0;
    }
  int iret = GranuleCheck::FEMClockCounterCheck();
  return iret;

} 

int
CheckMpc::FEMGL1ClockCounterCheck()
{
  if (! p->iValue(0, "NRMODULES")) // check for empty packet
    {
      packetiter->LastClock = -1;
      return 0;
    }
  int iret = GranuleCheck::FEMGL1ClockCounterCheck();
  return iret;
}

