#include "CheckErt.h"
#include "GranuleCheckDefs.h"

#include <cstdlib>
#include <cstring>

using namespace std;

CheckErt::CheckErt(const string &arm)
{
  if (arm == "WEST")
    {
      ThisName = "ERTWEST";
      GranuleNumber = GRAN_NO_ERT_WEST;
    }
  else if (arm == "EAST")
    {
      ThisName = "ERTEAST";
      GranuleNumber = GRAN_NO_ERT_EAST;
    }
  else
    {
      cout << "CheckErt: bad arm: " << arm
	   << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckErt::Init()
{
  struct packetstruct newpacket;
  int ipkt;
  if (!strcmp(Name(), "ERTWEST"))
    {
      ipkt = 14200;
    }
  else if (!strcmp(Name(), "ERTEAST"))
    {
      ipkt = 14201;
    }
  else
    {
      cout << "CheckErt: Bad ERT: " << Name() << endl;
      return -1;
    }
  newpacket.PacketId = ipkt;
  pkt.push_back(newpacket);
  num_all_packets = pkt.size();
  return 0;
}

int CheckErt::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}
