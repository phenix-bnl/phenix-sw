#include "CheckVtxp.h"
#include "GranuleCheckDefs.h"
#include "RawDataCheckDefs.h"

#include <packet.h>

#include <cstdlib>
#include <cstring>

using namespace std;

CheckVtxp::CheckVtxp(const string &arm):
  savebeamclk03(-1),
  savebeamclk47(-1)
{
  if (arm == "VTXP")
    {
      ThisName = "VTXP";
    }
  else
    {
      cout << "invalid name: " << arm << endl;
      exit(-2);
    }
  return ;
}

int CheckVtxp::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  if (ThisName == "VTXP")
    {
      GranuleNumber = GRAN_NO_VTXP;
      ipktmin = 24001;
      ipktmax = 24060;
    }
  else
    {
      cout << "invalid subsystem name: " << Name() << endl;
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
int
CheckVtxp::DcmFEMParityErrorCheck()
{
  if(p->iValue(0, "PARITYOK"))
    {
      return 0;
    }
  else
    {
      packetiter->DcmFEMParityError++;
    }
  return -1;
}

int
CheckVtxp::GlinkCheck()
{
  packetiter->GlinkError = -1;
  return 0;
}

void
CheckVtxp::SetBeamClock()
{
  BeamClock = p->iValue(0, "BCLCK");
  return;
}

int
CheckVtxp::LocalEventNumberOffset()
{
  return 0;
}

int
CheckVtxp::SubsystemCheck(Event *evt, const int iwhat)
{
  int iret = 0;
  if (iwhat != SUBSYS_PACKETEXIST)
    {
      return iret;
    }
  neventssubsyscheck[packetiter->PacketId]++;
  // chuck's status word, lower 4 bits are for number of buffered events
  int status = ((p->iValue(0, "STATUS")) & 0xFFF0);

  if(status)
    {
      if (verbosity > 0)
	{
	  cout << ThisName << ": Packet: " << packetiter->PacketId
	       << " Subsystem Error: status 0x" << hex << status
	       << dec << endl;
	}
      packetiter->SubsystemError++;
      iret = RawChk::SUBSYSCHK;
    }
  // check whats wrong with the status word
  int spiroevncnt = p->iValue(0, "SPIROEVENTCOUNT");
  if (spiroevncnt > 4)
    {
      if (verbosity > 0)

	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << " multi event buffering shot, buffered events: " << spiroevncnt
	       << endl;
	}
      spiroevtcnt[packetiter->PacketId]++;
    }
  if (p->iValue(0, "SPIRO_A_LOSSLOCK"))
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << " spiro a lossed lock" << endl;
	}
      lockloss_a[packetiter->PacketId]++;
    }
  if (p->iValue(0, "SPIRO_B_LOSSLOCK"))
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << " spiro b lossed lock" << endl;
	}
      lockloss_b[packetiter->PacketId]++;
    }
  if (p->iValue(0, "SPIRO_A_SIZEERROR"))
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << " spiro a size error" << endl;
	}
      sizerr_a[packetiter->PacketId]++;
    }
  if (p->iValue(0, "SPIRO_B_SIZEERROR"))
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << " spiro b size error" << endl;
	}
      sizerr_b[packetiter->PacketId]++;
    }
  if (p->iValue(0, "SPIROPARITYERROR"))
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << " spiro parity error" << endl;
	}
      parityerr[packetiter->PacketId]++;
    }
  // now check event numbers of spiro boards
  int spiroevt03 = p->iValue(0, "EVTNR03");
  int spiroevt47 = p->iValue(0, "EVTNR47");
  if (spiroevt03 != spiroevt47)
    {
      if (verbosity > 0)
	{
	  cout << ThisName << ": Packet: " << packetiter->PacketId
	       << "spiro event number mismatch" << hex
	       << " spiroevt0-3: 0x" << spiroevt03 
	       << " spiroevt4-7: 0x" << spiroevt47 
	       << dec << endl;
	}
      iret = RawChk::SPIROEvent;
    }
  int beamclk03 = p->iValue(0, "S_BCLCK03");
  int beamclk47 = p->iValue(0, "S_BCLCK47");
  int negbeamclk03 = p->iValue(0, "NEGBCLK03");
  int negbeamclk47 = p->iValue(0, "NEGBCLK47");
  if (beamclk03 & negbeamclk03)
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << hex << " beam clk 03: 0x" << beamclk03
	       << " neg: 0x" << negbeamclk03
	       << dec << endl;
	}
      map_beamclk03[packetiter->PacketId]++;
      iret = RawChk::SPIROClock03;
    }
  if (beamclk47 & negbeamclk47)
    {
      if (verbosity > 0)
	{
	  cout <<  ThisName << ": Packet: " << packetiter->PacketId
	       << hex << " beam clk 47: 0x" << beamclk47
	       << " neg: 0x" << negbeamclk47
	       << dec << endl;
	}
      map_beamclk47[packetiter->PacketId]++;
      iret = RawChk::SPIROClock47;
    }
  return iret;
}

int
CheckVtxp::EndRun(const int runno)
{
  map<int, int>::const_iterator iter;
  if (!spiroevtcnt.empty())
    {
      cout << "Spiro Event Counter exceeds multi event buffering (4): " << endl;
      for (iter = spiroevtcnt.begin(); iter != spiroevtcnt.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!lockloss_a.empty())
    {
      cout << "Lock Losses Spiro A: " << endl;
      for (iter = lockloss_a.begin(); iter != lockloss_a.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!lockloss_b.empty())
    {
      cout << "Lock Losses Spiro B: " << endl;
      for (iter = lockloss_b.begin(); iter != lockloss_b.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!sizerr_a.empty())
    {
      cout << "Size Error Spiro A: " << endl;
      for (iter = sizerr_a.begin(); iter != sizerr_a.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!sizerr_b.empty())
    {
      cout << "Size Error Spiro B: " << endl;
      for (iter = sizerr_b.begin(); iter != sizerr_b.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!parityerr.empty())
    {
      cout << "Parity Error: " << endl;
      for (iter = parityerr.begin(); iter != parityerr.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!map_beamclk03.empty())
    {
      cout << "Negated Beam Clock (0-3) Error: " << endl;
      for (iter = map_beamclk03.begin(); iter != map_beamclk03.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (!map_beamclk47.empty())
    {
      cout << "Negated Beam Clock (4-7) Error: " << endl;
      for (iter = map_beamclk47.begin(); iter != map_beamclk47.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  return 0;
}
