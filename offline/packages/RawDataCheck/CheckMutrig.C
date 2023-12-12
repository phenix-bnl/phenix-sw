#include "CheckMutrig.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <Event.h>
#include <packet.h>

#include <cstdlib>

using namespace std;

CheckMutrig::CheckMutrig(const string &arm)
{
  if (arm == "NORTH")
    {
      ThisName = "MUTRIGNORTH";
    }
  else if (arm == "SOUTH")
    {
      ThisName = "MUTRIGSOUTH";
    }
  else
    {
      cout << "invalid arm: " << arm << endl;
      exit(-2);
    }
  return ;
}

int 
CheckMutrig::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  if (ThisName == "MUTRIGNORTH")
    {
      GranuleNumber = GRAN_NO_MUTR_NORTH;
      ipktmin = 11500;
      ipktmax = 11503;
    }
  else if (ThisName == "MUTRIGSOUTH")
    {
      GranuleNumber = GRAN_NO_MUTR_SOUTH;
      ipktmin = 11510;
      ipktmax = 11513;
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

int CheckMutrig::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

unsigned int
CheckMutrig::LocalEventCounter()
{
  //  return p->iValue(0, "TRIGGER");
  return 0;
}

int
CheckMutrig::GlinkCheck()
{
  packetiter->GlinkError = -1;
  return 0;
}

int 
CheckMutrig::FEMEventSequenceCheck()
{
  packetiter->FEMEvtSeqError = -1;
  return 0;
}

int 
CheckMutrig::FEMGL1ClockCounterCheck()
{
  packetiter->FEMGL1ClockError = -1;
  return 0;
}

int 
CheckMutrig::FEMClockCounterCheck()
{
  packetiter->FEMClockCounterError = -1;
  return 0;
}
