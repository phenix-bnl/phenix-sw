#include "CheckMuid.h"
#include "GranuleCheckDefs.h"

#include <packet.h>

#include <cstdlib>
#include <cstring>
#include <iostream>

using namespace std;

CheckMuid::CheckMuid(const string &arm)
{
  if (arm == "SOUTH")
    {
      ThisName = "MUIDSOUTH";
      GranuleNumber = GRAN_NO_MUID_SOUTH;
    }
  else if (arm == "NORTH")
    {
      ThisName = "MUIDNORTH";
      GranuleNumber = GRAN_NO_MUID_NORTH;
    }
  else
    {
      cout << "CheckMuid: bad arm: " << arm
	   << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckMuid::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin = 0;
  unsigned int ipktmax = 0;
  if (!strcmp(Name(), "MUIDSOUTH"))
    {
      ipktmin = 12001;
      ipktmax = 12002;
    }
  else if (!strcmp(Name(), "MUIDNORTH"))
    {
      ipktmin = 12003;
      ipktmax = 12004;
    }
  else
    {
      cout << "CheckMuid: Bad MUID: " << Name() << endl;
      return -1;
    }
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  return 0;
}

int CheckMuid::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

int CheckMuid::SubsystemCheck(Event *evt, const int iwhat)
{
  if (iwhat != SUBSYS_PACKETEXIST)
    {
      return 0;
    }
  if (iwhat == SUBSYS_PACKETEXIST)
    {
      static const int GoodVal[3] =
        {
          0xFFFF, 0xFFFF, 0xFF
        };
      int error = 0;
      for (int i = 0;i < 3;i++)
        {
          if (p->iValue(i, "USERWORD") != GoodVal[i])
            {
              cout << "Bad MUID User Word " << i << ": "
		   << hex << p->iValue(i, "USERWORD") << dec << endl;
              error = -1;
            }
        }
      if (error)
        {
          totalsubsystemerror++;
          packetiter->SubsystemError++;
        }
      else
        {
          totalsubsystemerror = 0;
        }
      return error;
    }
  else if (iwhat == SUBSYS_PACKETMISS)
    {
      return MissingPacketCheck(evt);
    }
  return 0;

}
