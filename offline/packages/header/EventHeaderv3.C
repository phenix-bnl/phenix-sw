#include "EventHeaderv3.h"
#include <RawDataCheckDefs.h>

using namespace std;

ClassImp(EventHeaderv3)

EventHeaderv3::EventHeaderv3()
{
  Reset();
  return;
}

EventHeaderv3 *
EventHeaderv3::clone() const
{
  EventHeaderv3 * ret = new EventHeaderv3();

  ret->EvtSequence = this->EvtSequence;
  ret->EvtType = this->EvtType;
  ret->TimeStamp = this->TimeStamp;
  ret->badpackets = this->badpackets;

  return ret;
}

void
EventHeaderv3::Reset()
{
  EventHeaderv2::Reset(); // call reset of parent class
  badpackets.clear();
  return;
}

void
EventHeaderv3::identify(ostream& out) const
{
  out << "identify yourself: I am an EventHeaderv3 Object" << endl;
  out << "Event no: " << EvtSequence
      << ", Type: " << EvtType
      << ", ATP arrival time: " << ctime(&TimeStamp);
  set<unsigned int>::const_iterator biter;
  out << "Bad Packets for this event:" << endl;
  for (biter = badpackets.begin(); biter != badpackets.end(); ++biter)
    {
      unsigned int pkt = *biter;
      if ( (pkt & 0x00FF0000) ==  (RawChk::INFO << 16))
	{
	  continue;
	}
      out << (pkt & 0xFFFF) << ", reason ";
      // mask off upper bits which are reserved for non fatal subsystem info
      unsigned int error_bits = (pkt >> 16) & 0x00FF;
      switch(error_bits)
	{
	case RawChk::BADLEN:
	  cout << " Bad length";
	  break;
	case RawChk::DcmCheckSum:
	  cout << " Bad DcmCheckSum";
	  break;
	case RawChk::FEMParity:
	  cout << " Bad FEMParity";
	  break;
	case RawChk::DcmFEMParity:
	  cout << " Bad DcmFEMParity";
	  break;
	case RawChk::FEMClock:
	  cout << " Bad FEMClock";
	  break;
	case RawChk::FEMEvent:
	  cout << " Bad FEMEvent";
	  break;
	case RawChk::GL1Clock:
	  cout << " Bad GL1Clock";
	  break;
	case RawChk::SUBSYSCHK:
	  cout << " Bad SUBSYSCHK";
	  break;
	case RawChk::LDTBTIMEOUT:
	  cout << " LDTB TimeOut";
	  break;
	case RawChk::NOACTIVERCC:
	  cout << " No active RCC";
	  break;
	case RawChk::RCCCLOCK:
	  cout << " All RCCs Bad Clock";
	  break;
	case RawChk::BADCELLID:
	  cout << " mostly invalid Cell Ids";
	  break;
	case RawChk::MIXEDCELLID:
	  cout << " Too many different Cell Ids";
	  break;
	case RawChk::SPIROEvent:
	  cout << " SPIRO A/B Event number mismatch";
	  break;
	case RawChk::SPIROClock03:
	  cout << " SPIRO A Clock Problem";
	  break;
	case RawChk::SPIROClock47:
	  cout << " SPIRO B Clock Problem";
	  break;
	default:
	  cout << " bad subsystem status 0x" << hex <<  error_bits << dec
	       << " check RawDataCheckDefs.h for new entries";
	  break;
	}
      cout << endl;
    }
  return;
}

void
EventHeaderv3::AddBadPacket(const unsigned int ibad)
{
  set<unsigned int>::iterator piter = badpackets.find(ibad);
  if (piter == badpackets.end())
    {
      badpackets.insert(ibad);
    }
  else
    {
      cout << PHWHERE << " Paket " << ibad
	   << " allready added" << endl;
    }
  return;
}

int
EventHeaderv3::isBadPacket(const unsigned int ibad) const
{
  set<unsigned int>::const_iterator biter;
  for (biter = badpackets.begin(); biter != badpackets.end(); ++biter)
    {
      unsigned int pkt = (*biter) & 0xFFFF ; // get the lower 16 bits for the packet id
      if (pkt == ibad)
	{
	  // this is a kludge to guarantee backward compatibility
	  // if packets have the upper bits set (discarding a cellid)
	  // we don't want this packet to show up bad but if
	  // the error bits are set it is bad (even if we give it
	  // some info in the upper bits)
	  // initially we stored only the bad packets without
	  // error info
	  // so if all the upper bits are 0, it is a bad packet
	  unsigned int subsystembits = (*biter) >> 16; // upper 16 bits
	  unsigned int error_bits = subsystembits & 0xFF;
	  unsigned int info_bits = subsystembits & 0xFF00;
	  if ((error_bits && error_bits!=RawChk::INFO)  || (!error_bits && !info_bits))
	    {
	      return 1;
	    }
	}
        }
    return 0;
}

int
EventHeaderv3::GetPacketInfo(const unsigned int id, std::set<unsigned int> &returnset) const
{
  unsigned int entries = 0;
  set<unsigned int>::const_iterator biter;
  for (biter = badpackets.begin(); biter != badpackets.end(); ++biter)
    {
      unsigned int pkt = (*biter) & 0xFFFF ; // get the lower 16 bits for the packet id
      if (pkt == id)
	{
	  if ( ((*biter) & 0x00FF0000) !=  (RawChk::INFO << 16))
	    {
	      continue;
	    }
	  unsigned int info_bits = ((*biter) & 0xFF000000) >> 24;
	  returnset.insert(info_bits);
	  entries++;
	}
    }
  return entries;
}

