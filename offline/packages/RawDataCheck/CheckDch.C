#include "CheckDch.h"
#include "GranuleCheckDefs.h"

#include <cstdlib>
#include <cstring>

using namespace std;

CheckDch::CheckDch(const string &arm)
{
  if (arm == "WEST")
    {
      ThisName = "DCHWEST";
      GranuleNumber = GRAN_NO_DCH_WEST;
    }
  else if (arm == "EAST")
    {
      ThisName = "DCHEAST";
      GranuleNumber = GRAN_NO_DCH_EAST;
    }
  else
    {
      cout << "CheckDch: bad arm: " << arm 
	   << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckDch::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin = 0;
  unsigned int ipktmax = 0;
  if (!strcmp(Name(), "DCHWEST"))
    {
      ipktmin = 3081;
      ipktmax = 3160;
    }
  else if (!strcmp(Name(), "DCHEAST"))
    {
      ipktmin = 3001;
      ipktmax = 3080;
    }
  else
    {
      cout << "CheckDch: Bad DCH: " << Name() << endl;
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

int CheckDch::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}
