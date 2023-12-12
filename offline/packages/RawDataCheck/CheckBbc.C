#include "CheckBbc.h"
#include "GranuleCheckDefs.h"

#include <Event.h>

using namespace std;

CheckBbc::CheckBbc(const string &name):
  GranuleCheck("BBC")
{
  GranuleNumber = GRAN_NO_BBC;
  return ;
}

int CheckBbc::Init()
{
  struct packetstruct newpacket;
  for (unsigned int ipkt = 1001; ipkt <= 1003; ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  twobbcpkts = -1;
  return 0;
}

int CheckBbc::FEMClockCounterCheck()
{
  if (packetiter->PacketId == 1001)
    {
      packetiter->FEMClockCounterError = -1;
    }
  else
    {
      return (GranuleCheck::FEMClockCounterCheck());
    }
  return 0;
}

int 
CheckBbc::FEMParityErrorCheck()
{
  if (packetiter->PacketId == 1001)
    {
      return (GranuleCheck::FEMParityErrorCheck());
    }
  else
    {
      packetiter->FEMParityError = -1;
    }
  return 0;
}


int CheckBbc::SubsystemCheck(Event *evt, int iwhat)
{
  if (iwhat != SUBSYS_PACKETMISS)
    {
      return 0;
    }
  switch (twobbcpkts)
    {
    case 1:
      if (packetiter->PacketId == 1001)
        {
          return 0;
        }
      break;
    case 0:
      if (packetiter->PacketId == 1002 || packetiter->PacketId == 1003)
        {
          return 0;
        }
      break;
    default:
      break;
    }
  return MissingPacketCheck(evt);
}

int
CheckBbc::DoEveryEvent(Event *evt)
{
  switch(twobbcpkts)
    {
    case 1:
    case 0:
      return 0;
    }
  if (evt->existPacket(1001))
    {
      num_all_packets = 1;
      twobbcpkts = 0;
    }
  else if (evt->existPacket(1002) || evt->existPacket(1003))
    {
      num_all_packets = 2;
      twobbcpkts = 1;
    }
  return 0;
}
