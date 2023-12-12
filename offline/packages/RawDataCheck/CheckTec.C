#include "CheckTec.h"
#include "GranuleCheckDefs.h"
#include <packet.h>

using namespace std;

CheckTec::CheckTec():
  GranuleCheck("TEC")
{
  GranuleNumber = GRAN_NO_TEC;
  return ;
}

int CheckTec::Init()
{
  struct packetstruct newpacket;
  for (unsigned int ipkt = 5001; ipkt <= 5320;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();

  return 0;
}

unsigned int CheckTec::LocalEventCounter()
{
  unsigned int ival = p->iValue(0, "EVTNR") + 1;
  if (ival == 0x10000)
    {
      ival = 0;
    }
  return ival;
}

int CheckTec::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}
