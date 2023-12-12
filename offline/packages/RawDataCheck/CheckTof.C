#include "CheckTof.h"
#include "GranuleCheckDefs.h"

#include <phool.h>

#include <cstdlib>
#include <cstring>

using namespace std;

CheckTof::CheckTof(const char *arm)
{
  if (!strcmp(arm, "EAST"))
    {
      ThisName = "TOFEAST";
      GranuleNumber = GRAN_NO_TOF_EAST;
    }
  else if (!strcmp(arm, "WEST"))
    {
      ThisName = "TOFWEST";
      GranuleNumber = GRAN_NO_TOF_WEST;
    }
  else
    {
      cout << "DaqTofMon: bad TOF arm/System: " << arm
	   << " exiting now" << endl;
      exit(1);
    }

  return ;
}

int 
CheckTof::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  if (!strcmp(Name(), "TOFEAST"))
    {
      ipktmin = 7001;
      ipktmax = 7008;
    }
  else if (!strcmp(Name(), "TOFWEST"))
    {
      ipktmin = 7101;
      ipktmax = 7104;
    }
  else
    {
      cout << PHWHERE << " The impossible happened, bad subsystem: "
	   << Name() << endl;
      cout << "Send Mail to off-l with your macro" << endl;
      exit(1);
    }
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  ResetEvent(); // init internal variables
  return 0;
}

int 
CheckTof::FEMClockCounterCheck()
{
  if ((packetiter->PacketId >= 7001 && packetiter->PacketId <= 7004)
      || (packetiter->PacketId >= 7101 && packetiter->PacketId <= 7102))
    {
      if (standard_clock_1 == -1)
        {
          standard_clock_1 = BeamClock;
        }
      else
        {
          if ((int) BeamClock != standard_clock_1)
            {
              packetiter->FEMClockCounterError++;
              return -1;
            }
        }
    }
  else
    {
      if (standard_clock_2 == -1)
        {
          standard_clock_2 = BeamClock;
        }
      else
        {
          if ((int) BeamClock != standard_clock_2)
            {
              packetiter->FEMClockCounterError++;
              return -1;
            }
        }
    }
  return 0;
}

int 
CheckTof::ResetEvent()
{
  standard_clock_1 = -1;
  standard_clock_2 = -1;
  return 0;
}
