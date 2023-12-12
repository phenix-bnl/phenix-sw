#include "CheckGl1.h"
#include "GranuleCheckDefs.h"

#include <Event.h>

using namespace std;

CheckGl1::CheckGl1(const string &name):
  GranuleCheck(name)
{
  GranuleNumber = GRAN_NO_GL1;
  return ;
}

int CheckGl1::Init()
{
  struct packetstruct newpacket;
  newpacket.PacketId = 14001;
  pkt.push_back(newpacket);
  newpacket.PacketId = 14002;
  pkt.push_back(newpacket);
  newpacket.PacketId = 14008;
  pkt.push_back(newpacket);
  newpacket.PacketId = 14009;
  pkt.push_back(newpacket);
  newpacket.PacketId = 14011;
  pkt.push_back(newpacket);
  newpacket.PacketId = 14020;
  pkt.push_back(newpacket);
  newpacket.PacketId = 14021;
  pkt.push_back(newpacket);
  num_all_packets = pkt.size();

  return 0;
}

int 
CheckGl1::FEMParityErrorCheck()
{
      packetiter->FEMParityError = -1;
  return 0;
}


int CheckGl1::SubsystemCheck(Event *evt, int iwhat)
{
  if (iwhat != SUBSYS_PACKETMISS)
    {
      return 0;
    }
  return MissingPacketCheck(evt);
}

 int
 CheckGl1::process_event(Event *e)
 {
   return 0;
 }
