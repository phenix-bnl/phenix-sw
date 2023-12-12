#include "CheckFcal.h"
#include "GranuleCheckDefs.h"

#include <phool.h>

#include <cstdlib>
#include <cstring>

using namespace std;

CheckFcal::CheckFcal(const string &what)
{
  if (what == "FCAL")
    {
      ThisName = "FCAL";
      GranuleNumber = GRAN_NO_FCAL;
   }
  else if (what == "OLDMPC")
    {
      ThisName = "OLDMPC";
      GranuleNumber = GRAN_NO_FCAL;
    }
  else if (what == "MPC")
    {
      ThisName = "OLDMPC";
      GranuleNumber = GRAN_NO_FCAL;
    }
  else if (what == "RXNP")
    {
      ThisName = "RXNP";
      GranuleNumber = GRAN_NO_FCAL;
    }
  else
    {
      cout << PHWHERE << " Unknown granule " << what 
	   << " exiting now" << endl;
      exit(1);
    }
  return ;
}

int CheckFcal::Init()
{
  unsigned int ipktmin;
  unsigned int ipktmax;
  struct packetstruct newpacket;
  if (ThisName == "FCAL")
    {
      ipktmin = 16001;
      ipktmax = 16002;
    }
  else if (ThisName == "OLDMPC")
    {
      ipktmin = 21011;
      ipktmax = 21042;
    }
  else if (ThisName == "RXNP")
    {
      ipktmin = 23001;
      ipktmax = 23001;
    }
  else
    {
      cout << PHWHERE 
           << "This cannot happen, please notify off-l and send a copy of your macro" 
	   << " Name: " << ThisName << endl;
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

int CheckFcal::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}
