#include "CheckMutr.h"
#include "RawDataCheck.h"
#include "GranuleCheckDefs.h"

#include <packet.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>

#include <cstdlib>
#include <cstring>

using namespace std;

CheckMutr::CheckMutr(const string &arm)
{
  if (arm == "SOUTH")
    {
      ThisName = "MUTRSOUTH";
      GranuleNumber = GRAN_NO_MUTR_SOUTH;
    }
  else if (arm == "NORTH")
    {
      ThisName = "MUTRNORTH";
      GranuleNumber = GRAN_NO_MUTR_NORTH;
    }
  else
    {
      cout << "CheckMutr: bad arm: " << arm
	   << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckMutr::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin = 0;
  unsigned int ipktmax = 0;
  if (!strcmp(Name(), "MUTRSOUTH"))
    {
      ipktmin = 11001;
      ipktmax = 11168;
    }
  else if (!strcmp(Name(), "MUTRNORTH"))
    {
      ipktmin = 11171;
      ipktmax = 11362;
    }
  else
    {
      cout << "CheckMutr: Bad MUTR: " << Name() << endl;
      return -1;
    }
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  ResetEvent();
  return 0;
}

int
CheckMutr::BeginRun(const int runno)
{
  runnumber = runno;
  return 0;
}

int
CheckMutr::FEMClockCounterCheck()
{
  int iarray = p->iValue(0, "MODULE");
  if (iarray > 1 || iarray < 0)
    {
      int beamclk_ok = 0;
      // even if module is off, still try to check beam clock
      for (int n = 0; n < 2;n++)
        {
          if (standard_clock[n] != -1)
            {
              if (standard_clock[n] == (int) BeamClock)
                {
                  beamclk_ok = 1;
                }
            }
          else
            {
              beamclk_ok = 1;
            }
        }
      if (verbosity > 0 && beamclk_ok == 0)
        {
          packetiter->FEMClockCounterError++;
	    cout << "PacketId: " << packetiter->PacketId
	    << ", BeamClock: 0x" << hex << BeamClock
	    << ", standard_clock[0]: 0x"
	    << standard_clock[0] 
	    << ", standard_clock[1]: 0x"
	    << standard_clock[1] 
            << dec << endl;
          return -1;
        }
      else
        {
          return 0;
        }
    }
  if ( standard_clock[iarray] == -1)
    {
      standard_clock[iarray] = BeamClock;
    }
  else
    {
      if (standard_clock[iarray] != (int) BeamClock)
        {
          packetiter->FEMClockCounterError++;
          /*
	    cout << "PacketId: " << packetiter->PacketId
	    << ", BeamClock: " << hex << BeamClock
	    << ", standard_clock[" << iarray << "]: "
	    << standard_clock[iarray] << dec << endl;
          */
          return -1;
        }
    }
  return 0;
}

int
CheckMutr::ResetEvent()
{
  for (int i = 0; i < 2;i++)
    {
      standard_clock[i] = -1;
    }
  return 0;
}

int
CheckMutr::FEMGL1ClockCounterCheck()
{
  if (runnumber < (int) BEGIN_OF_RUN5 && runnumber > (int) BEGIN_OF_RUN4 && rawdatacheck->MultiBuffered())
    {
      packetiter->LastClock = -1;
      return 0;
    }
  int iret = GranuleCheck::FEMGL1ClockCounterCheck();
  return iret;
}

int
CheckMutr::SubsystemCheck(Event *evt, const int iwhat)
{
  if (iwhat != SUBSYS_PACKETEXIST)
    {
      return 0;
    }
  int iarray = p->iValue(0, "MODULE");
  if (iarray < 0 || iarray > 1)
    {
      //      cout << "Invalid Module address from packet " << packetiter->PacketId << endl;
      packetiter->SubsystemError++;
      return -1;
    }
  return 0;
}
