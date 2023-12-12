#include "CheckRpc.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <Event.h>
#include <packet.h>

#include <cstdlib>

using namespace std;

CheckRpc::CheckRpc(const string &arm)
{
  if (arm == "RPC")
    {
      ThisName = "RPC3";
    }
  if (arm == "RPC3")
    {
      ThisName = "RPC3";
    }
  else if (arm == "RPC1")
    {
      ThisName = "RPC1";
    }
  else
    {
      cout << "invalid arm: " << arm << endl;
      exit(-2);
    }
  return ;
}

int 
CheckRpc::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  if (ThisName == "RPC")
    {
      GranuleNumber = GRAN_NO_RPC3;
      ipktmin = 19001;
      ipktmax = 19008;
    }
  else if (ThisName == "RPC3")
    {
      GranuleNumber = GRAN_NO_RPC3;
      ipktmin = 19001;
      ipktmax = 19008;
    }
  else if (ThisName == "RPC1")
    {
      GranuleNumber = GRAN_NO_RPC1;
      ipktmin = 19009;
      ipktmax = 19012;
    }
  else
    {
      cout << "invalid subsystem name: " << Name() << endl;
      exit(1);
    }
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax; ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();

  return 0;
}

int CheckRpc::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

unsigned int
CheckRpc::LocalEventCounter()
{
  int iev = p->iValue(0, "EVTNR");
  iev++;
  return (iev&0xFFF); // 12 bit event number
}

int
CheckRpc::GlinkCheck()
{
  packetiter->GlinkError = -1;
  return 0;
}

int
CheckRpc::FEMEventSequenceCheck()
{
  int local_event_counter = LocalEventCounter();
  if (!rawdatacheck->GL1exists())
    {
      if (local_event_counter != ((rawdatacheck->EventNumber() + LocalEventNumberOffset()) & 0xFFF))
        {
          if (verbosity > 0)
            {
              cout << ThisName << "  <E> FEMEventSequenceCheck: "
		   << "Packet: " << packetiter->PacketId
		   << ", local_event_counter: 0x" << hex << local_event_counter
		   << ", rawdatacheck->EventNumber() + LocalEventNumberOffset(): 0x"
		   << ((rawdatacheck->EventNumber() + LocalEventNumberOffset())  & 0xFFF)
		   << dec << endl;
            }
          packetiter->FEMEvtSeqError++;
          return -1;
        }
    }
  else
    {
      if ((local_event_counter&0xFFFF) != ((int) (GlobalEventCounter()&0xFFF)))
        {
          if (verbosity > 0)
            {
              cout << ThisName << " <E> FEMEventSequenceCheck: "
		   << "Packet: " << packetiter->PacketId
		   << ", local_event_counter: 0x" << hex << local_event_counter
		   << ", GlobalEventCounter(): 0x"
		   << (GlobalEventCounter()&0xFFF) << dec << endl;
            }
          packetiter->FEMEvtSeqError++;
          return -1;
        }
    }
  return 0;
}

void
CheckRpc::SetBeamClock()
{
  BeamClock = p->iValue(0, "CLOCK");
  return;
}

// int 
// CheckRpc::FEMEventSequenceCheck()
// {
//   packetiter->FEMEvtSeqError = -1;
//   return 0;
// }

// int 
// CheckRpc::FEMGL1ClockCounterCheck()
// {
//   packetiter->FEMGL1ClockError = -1;
//   return 0;
// }

// int 
// CheckRpc::FEMClockCounterCheck()
// {
//   packetiter->FEMClockCounterError = -1;
//   return 0;
// }
