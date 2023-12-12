#include <CheckAcc.h>
#include <GranuleCheckDefs.h>

using namespace std;

CheckAcc::CheckAcc():
  GranuleCheck("ACC")
{
  GranuleNumber = GRAN_NO_ACC;
  return ;
}

int 
CheckAcc::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin[2];
  unsigned int ipktmax[2];
  ipktmin[0] = 17001;
  ipktmax[0] = 17002;
  ipktmin[1] = 17005;
  ipktmax[1] = 17006;
  for (unsigned int j=0; j<2; j++)
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

int 
CheckAcc::DcmFEMParityErrorCheck()
{
  packetiter->DcmFEMParityError = -1;
  return 0;
}

int
CheckAcc::FEMParityErrorCheck()
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
