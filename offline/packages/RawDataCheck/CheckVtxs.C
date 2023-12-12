#include "CheckVtxs.h"
#include "GranuleCheckDefs.h"
#include "RawDataCheckDefs.h"
#include "RawDataCheck.h"

#include <packet.h>

#include <cmath>
#include <cstdlib>
#include <cstring>

using namespace std;

#define MAXCELLID 46
#define MINCELLID 1

CheckVtxs::CheckVtxs(const string &arm)
{
  if (arm == "VTXS")
    {
      ThisName = "VTXS";
    }
  else
    {
      cout << "invalid name: " << arm << endl;
      exit(-2);
    }
  return ;
}

int CheckVtxs::Init()
{
  struct packetstruct newpacket;
  unsigned int ipktmin;
  unsigned int ipktmax;
  if (ThisName == "VTXS")
    {
      GranuleNumber = GRAN_NO_VTXS;
      ipktmin = 24101;
      ipktmax = 24140;
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
CheckVtxs::DcmFEMParityErrorCheck()
{
  // copied from online monitoring
  int fem_parity = p->iValue(0, "PARITY"); // parity of packet sent by FEM
  int dcm_parity = p->iValue(0, "DCMPARITY"); // parity of the packet received by the DCM

  if ( fem_parity == dcm_parity )
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
CheckVtxs::GlinkCheck()
{
  packetiter->GlinkError = -1;
  return 0;
}

void
CheckVtxs::SetBeamClock()
{
  BeamClock = p->iValue(0, "BCLCK");
  return;
}

unsigned int
CheckVtxs::LocalEventCounter()
{
  // event counter starts at 0
  int iev = p->iValue(0, "EVTNR");
  iev++;
  return iev; // 12 bit event number
}

int
CheckVtxs::SubsystemCheck(Event *evt, const int iwhat)
{
  int iret = 0;
  if (iwhat != SUBSYS_PACKETEXIST)
    {
      return iret;
    }
  map<int , unsigned int > cellids;
  neventssubsyscheck[packetiter->PacketId]++;
  int flagword = p->iValue(0, "FLAG");
  if (flagword & 0x80) // 7th bit is LDTB timeout bit -> communication timeout with ladder -> bad
    {
      if (verbosity > 0)
	{
	  cout << "Packet " << packetiter->PacketId
	       << " LDTB timeout returning LDTBTIMEOUT error"
	       << endl;
	}
      flag_bit[packetiter->PacketId]++;
      return RawChk::LDTBTIMEOUT; // if we have a timeout we do not go through the packet
    }
  int rccenabled[6];
  unsigned int number_active_rcc = 0;
  for (int rcc = 0; rcc < 6; rcc++)
    {
      rccenabled[rcc] = p->iValue(rcc, "ENABLED");
      if (rccenabled[rcc])
	{
	  ++number_active_rcc;
	}
    }
  // if everything is disabled no point in checking anything
  if (number_active_rcc == 0)
    {
      if (verbosity > 0)
	{
	  cout << "Packet " << packetiter->PacketId
	       << " no active rccs"
	       << " returning NOACTIVERCC error"
	       << endl;
	}
      return RawChk::NOACTIVERCC;
    }
  // check all RCC beam clocks
  vector<int> rccfailbclock;
  vector<unsigned int> badstatus;
  int beamclockcounter = p->iValue(0, "BCLCK");
  for (int rcc = 0; rcc < 6; rcc++)
    {
      if (rccenabled[rcc])
	{
	  if (  fabs(p->iValue(rcc, "RCCBCLK") - beamclockcounter) > 1)
	    {
	      if (verbosity > 1)
		{
		  cout << "packet " << packetiter->PacketId
		       << " beamclock 0x" << hex << beamclockcounter
		       << ", rcc " << rcc << ": 0x"
		       <<  p->iValue(rcc, "RCCBCLK")
		       << dec << endl;
		}
	      rccfailbclock.push_back(rcc);
	      int index = packetiter->PacketId * 10 + rcc;
	      rccbadbeamclk[index]++;
	    }
	}
    }
  // we have rcc beam clock failures
  if (rccfailbclock.size() > 0)
    {
      if (rccfailbclock.size() == number_active_rcc) // if all of them failed we bail out here
	{
	  if (verbosity > 0)
	    {
	      cout << "Packet " << packetiter->PacketId << " all "
		   << number_active_rcc << " active rccs failed clock counter check "
		   << " returning RCCCLOCK error"
		   << endl;
	    }
	  return RawChk::RCCCLOCK;
	}
      vector<int>::const_iterator iter;
      for (iter = rccfailbclock.begin(); iter != rccfailbclock.end(); ++iter)
	{
	  rccenabled[*iter] = 0; // disable it for the cell id check, we don't care
	  // we have 16 bits for non packet info. The lower 8 bits are reserved for
	  // errors which invalidate the packet, the upper 8 bits for more info
	  // we have 8 bits for subsystem info, make it simple - upper 4 bits are the rcc, lower 4 bits are the cellids
	  // here we mark the lower 4 bits with 0xF to indicate that it is a clock error
	  unsigned int status = *iter << 12;
	  status += 0xF << 8;
	  status += RawChk::INFO; // mark this as an info entry (non fatal packet error)
	  badstatus.push_back(status);
	}
    }
  unsigned int number_active_chips = 0;
  map<unsigned int, set<unsigned int> > savechipcounts;
  for (unsigned int rcc = 0; rcc < 6; rcc++)
    {
      if (rccenabled[rcc])
	{
	  for (unsigned int chip = 0; chip < 12; chip++)
	    {
	      int cell_id = p->iValue(rcc, chip, "CELLID");
	      if (verbosity > 10)
		{
	          cout << "packet: " << packetiter->PacketId
		       << ", rcc: " << rcc
		       << ", chip: " << chip
                       << ", cell id: " <<  cell_id << endl;
		}
	      cellids[cell_id]++;
	      ++number_active_chips;
	      unsigned int rccchip = 100 * rcc + chip;
	      savechipcounts[cell_id].insert(rccchip);
	    }
	}
    }
  if (cellids.size() == 1)
    {
      if (cellids.begin()->first > MAXCELLID || cellids.begin()->first < MINCELLID)
	{
	  if (verbosity > 0)
	    {
	      cout << "Packet " << packetiter->PacketId << " all chips have invalid cell id "
		   << cellids.begin()->first
		   << " returning BADCELLID error"
		   << endl;
	    }
	  return RawChk::BADCELLID;
	}
    }
  else if (cellids.size() > 1)
    {
      map<int, unsigned int>::const_iterator iter;
      int saveindex = -1;
      unsigned int maxcnt = 0;
      for (iter = cellids.begin(); iter != cellids.end(); iter++)
	{
	  if (iter->second > maxcnt)
	    {
	      maxcnt = iter->second;
	      saveindex = iter->first; // saveindex contains the cell id with the max counts
	    }
	}
      // if our maximum number of cell ids which agree is less than half of the total
      // number of cells we do not have a majority vote which cell is correct
      // drop this packet
      if (maxcnt < number_active_chips / 2)
	{
	  if (verbosity > 0)
	    {
	      cout << "Packet " << packetiter->PacketId << " has only " << maxcnt
		   << " less than half of " << number_active_chips
		   << " active chips, fraction "
		   << (float) maxcnt / (float) number_active_chips
		   << " returning MIXEDCELLID error"
		   << endl;
	    }
	  return RawChk::MIXEDCELLID;
	}
      if (saveindex > MAXCELLID || saveindex < MINCELLID) // more than <MAXCELLID> or less than <MINCELLID> has invalid cell id
	{
	  if (verbosity > 0)
	    {
	      cout << "Packet " << packetiter->PacketId << " majority of chips "
		   << cellids[saveindex] << " has invalid cell id "
		   << saveindex
		   << " returning BADCELLID error"
		   << endl;
	    }
	  return RawChk::BADCELLID;
	}
      cellidproblem[packetiter->PacketId]++;
      // okay lets figure out the status for the addbadpacket
      savechipcounts.erase(savechipcounts.find(saveindex)); // remove the one good entry which determines the good cell id
      // this kludge loops over the saved chip counts, fills an rcc wise map, then checks if
      // all chips on a given rcc are bad (if we have 12 entries, all chips have a bad cell id)
      // If all chips are bad we mark the rcc as bad and prevent single entries for every chip
      map<unsigned int, set<unsigned int> >::const_iterator mapiter;
      set<unsigned int>::const_iterator setiter;
      map<unsigned int, unsigned int> badrccchips;
      for (mapiter = savechipcounts.begin(); mapiter != savechipcounts.end(); ++mapiter)
      	{
      	  for (setiter = mapiter->second.begin(); setiter != mapiter->second.end(); setiter++)
       	    {
      	      unsigned int rccchip = *setiter;
       	      unsigned int rcc = rccchip / 100;
       	      badrccchips[rcc]++;
       	    }
       	}
       map<unsigned int, unsigned int>::const_iterator rciter;
       // if all 12 chips of an rcc fail with bad cellid, mark the rcc bad
       for (rciter = badrccchips.begin(); rciter != badrccchips.end(); ++rciter)
       	{
 	  if (rciter->second == 12)
       	    {
       	      unsigned int status = (rciter->first) << 12;
       	      status += 0xF << 8;
       	      status += RawChk::INFO; // mark this as an info entry (non fatal packet error)
       	      badstatus.push_back(status);
       	    }
       	}
       // loop over everything again and mark the single chips as bad
      for (mapiter = savechipcounts.begin(); mapiter != savechipcounts.end(); ++mapiter)
	{
	  for (setiter = mapiter->second.begin(); setiter != mapiter->second.end(); setiter++)
	    {
	      unsigned int rccchip = *setiter;
	      unsigned int rcc = rccchip / 100;
	      if (badrccchips[rcc] < 12) // exclude rccs with 12 bad chips
		{
		  unsigned int chip = rccchip - 100 * rcc;
		  unsigned int status = rcc << 12;
		  status += chip << 8;
		  status += RawChk::INFO; // mark this as an info entry (non fatal packet error)
		  badstatus.push_back(status);
		}
	    }
	}
    }
  if (!badstatus.empty())
    {
      vector<unsigned int>::const_iterator iter;
      for (iter = badstatus.begin(); iter != badstatus.end(); ++iter)
	{
	  if (verbosity > 0)
	    {
	      int chipbits = ((*iter) >> 8) & 0xF;
	      int rcc = (*iter) >> 12;
	      cout << "marking packet " << packetiter->PacketId
		   << " with info status 0x" << hex  << *iter << dec
		   << " that is rcc " << rcc;
	      if (chipbits != 0xF)
		{
		  cout << " cell id " << chipbits;
		}
	      cout << endl;
	    }
	  rawdatacheck->AddBadPacket(packetiter->PacketId, *iter);
	}
    }
  return iret;
}

int
CheckVtxs::EndRun(const int endrun)
{
  map<int, int>::const_iterator iter;
  if (flag_bit.size() > 0)
    {
      cout << "Flag word bit 7 set:" << endl;
      for (iter = flag_bit.begin(); iter != flag_bit.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  if (rccbadbeamclk.size() > 0)
    {
      cout << "RCC beam clock mismatch:" << endl;
      for (iter = rccbadbeamclk.begin(); iter != rccbadbeamclk.end(); iter++)
	{
	  int index =   iter->first / 10;
	  int rcc = iter->first - 10 * index;
	  cout << "Packet " << index << ", rcc " << rcc  << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[index] << endl;
	}
    }
  if (cellidproblem.size() > 0)
    {
      cout << "Cell Id mismatch:" << endl;
      for (iter = cellidproblem.begin(); iter != cellidproblem.end(); iter++)
	{
	  cout << "Packet " << iter->first << " Bad Events: " << iter->second
	       << " out of " << neventssubsyscheck[iter->first] << endl;
	}
    }
  return 0;
}
