#include "CheckEmc.h"
#include "GranuleCheckDefs.h"

#include <cstdlib>
#include <cstring>

using namespace std;

CheckEmc::CheckEmc(const string &arm, const string &sector)
{
  ThisName = "EMC";
  ThisName = ThisName + arm + sector;
  if (arm == "WEST")
    {
      if (sector == "TOP")
        {
          GranuleNumber = GRAN_NO_EMC_WEST_TOP;
        }
      else if (sector == "BOTTOM")
        {
          GranuleNumber = GRAN_NO_EMC_WEST_BOTTOM;
        }
      else
        {
          goto badparam;
        }
    }
  else if (arm == "EAST")
    {
      if (sector == "TOP")
        {
          GranuleNumber = GRAN_NO_EMC_EAST_TOP;
        }
      else if (sector == "BOTTOM")
        {
          GranuleNumber = GRAN_NO_EMC_EAST_BOTTOM;
        }
      else
        {
          goto badparam;
        }
    }
  else
    {
      goto badparam;
    }
  return ;
 badparam:
  cout << "CheckEmc: bad arm: " << arm << " or sector: " << sector
       << " valid is arm = EAST WEST, sector = TOP BOTTOM, initializing EASTTOP" << endl;
  ThisName = "EMCEASTTOP";
  GranuleNumber = GRAN_NO_EMC_EAST_TOP;
  return ;
}

int CheckEmc::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  unsigned int refchannelmin;
  unsigned int refchannelmax;
  if (!strcmp(Name(), "EMCWESTTOP"))
    {
      ipktmin = 8037;
      ipktmax = 8072;
      refchannelmin = 8174; 
      refchannelmax = 8174; 
    }
  else if (!strcmp(Name(), "EMCEASTTOP"))
    {
      ipktmin = 8073;
      ipktmax = 8108;
      refchannelmin = 8175; 
      refchannelmax = 8175; 
    }
  else if (!strcmp(Name(), "EMCWESTBOTTOM"))
    {
      ipktmin = 8001;
      ipktmax = 8036;
      refchannelmin = 8173; 
      refchannelmax = 8173; 
    }
  else if (!strcmp(Name(), "EMCEASTBOTTOM"))
    {
      ipktmin = 8109;
      ipktmax = 8172;
      refchannelmin = 8177; 
      refchannelmax = 8180; 
    }
  else
    {
      cout << "CheckEmc: Bad EMC: " << Name() << endl;
      return -1;
    }
  for (unsigned int ipkt = ipktmin; ipkt <= ipktmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  for (unsigned int ipkt = refchannelmin; ipkt <= refchannelmax;ipkt++)
    {
      newpacket.PacketId = ipkt;
      pkt.push_back(newpacket);
    }
  num_all_packets = pkt.size();
  return 0;
}

int CheckEmc::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}
