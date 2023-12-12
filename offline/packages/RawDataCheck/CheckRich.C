#include "CheckRich.h"
#include "GranuleCheckDefs.h"

#include <cstdlib>
#include <cstring>

using namespace std;

CheckRich::CheckRich(const string &arm)
{
  if (arm== "WEST")
    {
      ThisName = "RICHWEST";
      GranuleNumber = GRAN_NO_RICH_WEST;
      num_all_packets = 16;
    }
  else if (arm == "EAST")
    {
      ThisName = "RICHEAST";
      GranuleNumber = GRAN_NO_RICH_EAST;
      num_all_packets = 16;
    }
  else
    {
      cout << "CheckRich: bad arm: " << arm
	   << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckRich::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin[2];
  unsigned int ipktmax[2];
  unsigned short int jmax = 0;
  if (!strcmp(Name(), "RICHWEST"))
    {
      ipktmin[0] = 6001;
      ipktmax[0] = 6008;
      ipktmin[1] = 6011;
      ipktmax[1] = 6018;
      jmax = 2;
    }
  else if (!strcmp(Name(), "RICHEAST"))
    {
      ipktmin[0] = 6021;
      ipktmax[0] = 6028;
      ipktmin[1] = 6031;
      ipktmax[1] = 6038;
      jmax = 2;
    }
  else
    {
      cout << "CheckRich: Bad RICH: " << Name() << endl;
      return -1;
    }
  for (unsigned short j = 0; j < jmax; j++)
    {
      for (unsigned int ipkt = ipktmin[j]; ipkt <= ipktmax[j];ipkt++)
        {
          newpacket.PacketId = ipkt;
          pkt.push_back(newpacket);
        }
    }
  num_all_packets = pkt.size();

  return 0;
}

int
CheckRich::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

int
CheckRich::FEMParityErrorCheck()
{
  // FEM parity word is wrong for this hitformat
  if (hitformat == 1006)
    {
      packetiter->FEMParityError = -1;
      return 0;
    }
  int iret = GranuleCheck::FEMParityErrorCheck();
  return iret;
}
