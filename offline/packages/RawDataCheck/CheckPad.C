#include "CheckPad.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <cstdlib>
#include <cstring>

using namespace std;


// packet 4008 has a bit error in the clock

CheckPad::CheckPad(const string &arm)
{
  if (arm == "WEST")
    {
      ThisName = "PADWEST";
      GranuleNumber = GRAN_NO_PAD_WEST;
    }
  else if (arm == "EAST")
    {
      ThisName = "PADEAST";
      GranuleNumber = GRAN_NO_PAD_EAST;
    }
  else
    {
      cout << "CheckPad: bad arm: " << arm
           << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckPad::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin[3];
  unsigned int ipktmax[3];
  unsigned short int jmax = 0;
  if (!strcmp(Name(), "PADWEST"))
    {
      ipktmin[0] = 4001;
      ipktmax[0] = 4016;
      ipktmin[1] = 4033;
      ipktmax[1] = 4048;
      ipktmin[2] = 4065;
      ipktmax[2] = 4080;
      jmax = 3;
    }
  else if (!strcmp(Name(), "PADEAST"))
    {
      ipktmin[0] = 4017;
      ipktmax[0] = 4032;
      ipktmin[1] = 4081;
      ipktmax[1] = 4096;
      jmax = 2;
    }
  else
    {
      cout << "CheckPad: Bad PAD: " << Name() << endl;
      return -1;
    }
  for (unsigned short j = 0; j < jmax; j++)
    {
      for (unsigned int ipkt = ipktmin[j]; ipkt <= ipktmax[j]; ipkt++)
        {
          newpacket.PacketId = ipkt;
          pkt.push_back(newpacket);
        }
    }
  num_all_packets = pkt.size();
  return 0;
}

int CheckPad::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

int
CheckPad::BadBeamClock()
{
  // pad chambers have a rollover problem in the beamcounter
  if (BeamClock == 0x0)
    {
      return -1;
    }
  return 0;
}

int
CheckPad::FEMClockCounterCheck4008()
{
  static unsigned int standard_clock;
  static int p4008first = 0;

  if (p4008first == 1)
    {
      standard_clock = BeamClock;
      p4008first = 0;
      return 0;
    }

  if (packets_found == 1)
    {
      if (packetiter->PacketId == 4008)
        {
          p4008first = 1;
          return 0;
        }
      standard_clock = BeamClock;
    }
  else
    {
      if (BeamClock != standard_clock)
        {
          if (packetiter->PacketId == 4008) // packet 4008 has bit error
            {
              if ((BeamClock | 0x20000) == (standard_clock | 0x20000))
                {
                  return 0;
                }
            }
          packetiter->FEMClockCounterError++;
          return -1;
        }
    }
  return 0;
}
