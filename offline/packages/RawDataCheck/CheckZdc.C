#include "CheckZdc.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

using namespace std;

CheckZdc::CheckZdc():
  GranuleCheck("ZDC")
{
  GranuleNumber = GRAN_NO_ZDC;
  return ;
}

int CheckZdc::Init()
{
  struct packetstruct newpacket;
  newpacket.PacketId = 13001;
  pkt.push_back(newpacket);
  num_all_packets = pkt.size();
  
  return 0;
}

int CheckZdc::FEMClockCounterCheck()
{
  packetiter->FEMClockCounterError = -1;
  return 0;
}

int CheckZdc::SubsystemCheck(Event *evt, const int iwhat)
{
  if (iwhat != SUBSYS_PACKETMISS)
    {
      return 0;
    }
  return MissingPacketCheck(evt);
}
