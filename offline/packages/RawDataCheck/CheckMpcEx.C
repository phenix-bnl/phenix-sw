#include "CheckMpcEx.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <Event.h>
#include <packet.h>

#include <cstdlib>

using namespace std;

CheckMpcEx::CheckMpcEx(const string &arm)
{
  if (arm == "NORTH")
    {
      ThisName = "MPCEXNORTH";
    }
  else if (arm == "SOUTH")
    {
      ThisName = "MPCEXSOUTH";
    }
  else
    {
      cout << "invalid arm: " << arm << endl;
      exit(-2);
    }
  return ;
}

int 
CheckMpcEx::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  if (ThisName == "MPCEXNORTH")
    {
      GranuleNumber = GRAN_NO_MPCEX_NORTH;
      ipktmin = 21301;
      ipktmax = 21308;
    }
  else if (ThisName == "MPCEXSOUTH")
    {
      GranuleNumber = GRAN_NO_MPCEX_SOUTH;
      ipktmin = 21351;
      ipktmax = 21358;
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

int CheckMpcEx::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

unsigned int
CheckMpcEx::LocalEventCounter()
{
  int iev = p->iValue(0, "EVTNR");
  iev++;
  return (iev&0xFFF); // 12 bit event number
}

void
CheckMpcEx::SetBeamClock()
{
  BeamClock = p->iValue(0, "BCLCK");
  return;
}

