#include "GranuleCheck.h"
#include "GranuleCheckDefs.h"
#include "RawDataCheck.h"
#include "RawDataCheckDefs.h"

#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>

#include <Event.h>
#include <EventTypes.h>

#include <TH2.h>

#include <cstdlib>
#include <iomanip>
#include <iostream>


using namespace std;

GranuleCheck::GranuleCheck(const string &name):
  ThisName(name),
  verbosity(0),
  totalgl1syncerror(0),
  totalsubsystemerror(0),
  num_all_packets(0),
  gl1syncok(0),
  rawdatacheck(NULL)
{
  return ;
}

int
GranuleCheck::Reset()
{
  PacketResetAll();
  Fun4AllServer *s = Fun4AllServer::instance();
  string hname = ThisName + "Monitor";
  TH1 *delhis = dynamic_cast<TH1*> (s->getHisto(hname.c_str()));
  delete delhis;
  ErrorCounterHisto = new TH2F(hname.c_str(), hname.c_str(), pkt.size(), 0, pkt.size(), NBINSPKT, 0., NBINSPKT );
  s->registerHisto(hname.c_str(), ErrorCounterHisto, 1);
  return 0;
}

void
GranuleCheck::identify(ostream& out) const
{
  static int iwid = 11;
  out << "GranuleCheck Object, Name: " << ThisName
      << " Granule No: " << GranuleNumber << endl;
  cout << "Number of packets: " << pkt.size() << endl;
  cout << "Error Counts: " << endl;
  cout << setw(iwid) << "Packet"
       << setw(iwid) << "Length"
       << setw(iwid) << "DcmChkSum"
       << setw(iwid) << "FEMParity"
       << setw(iwid) << "DcmFEMPar"
       << setw(iwid) << "FEMClk"
       << setw(iwid) << "FEMGL1Clk"
       << setw(iwid) << "FEMEvtSeq"
       << setw(iwid) << "GlinkErr"
       << setw(iwid) << "SubsysErr"
       << setw(iwid) << "NPackets"
       << setw(iwid) << "GoodPkt"
       << setw(iwid) << "Byt/pkt"
       << endl;

  vector<packetstruct>::const_iterator iter;
  for (iter = pkt.begin(); iter != pkt.end(); iter++)
    {

      int BytesPerPacket;
      if (iter->PacketCounter > 0)
        {
          BytesPerPacket = iter->SumBytes / iter->PacketCounter;
        }
      else
        {
          BytesPerPacket = 0;
        }
      cout << setw(iwid) << iter->PacketId
           << setw(iwid) << iter->BadLength
           << setw(iwid) << iter->DcmCheckSumError
           << setw(iwid) << iter->FEMParityError
           << setw(iwid) << iter->DcmFEMParityError
           << setw(iwid) << iter->FEMClockCounterError
           << setw(iwid) << iter->FEMGL1ClockError
           << setw(iwid) << iter->FEMEvtSeqError
           << setw(iwid) << iter->GlinkError
           << setw(iwid) << iter->SubsystemError
           << setw(iwid) << iter->PacketCounter
           << setw(iwid) << iter->GoodPackets
           << setw(iwid) << BytesPerPacket
           << endl;
    }
  return ;
}

int
GranuleCheck::CreateHisto()
{
  Fun4AllServer *s = Fun4AllServer::instance();
  string hname = ThisName + "Monitor";
  ErrorCounterHisto = new TH2F(hname.c_str(), hname.c_str(), pkt.size(), 0, pkt.size(), NBINSPKT, 0., NBINSPKT );
  s->registerHisto(hname.c_str(), ErrorCounterHisto);
  hname = ThisName + "PacketId";
  packetHisto = new TH1F(hname.c_str(), hname.c_str(), pkt.size(), 0, pkt.size());
  InitPacketIdHisto();
  s->registerHisto(hname.c_str(), packetHisto);
  hname = ThisName + "AllVariables";
  variableHisto = new TH1F(hname.c_str(), hname.c_str(), NBINSVAR, 0, NBINSVAR);
  s->registerHisto(hname.c_str(), variableHisto);
  return 0;
}

int
GranuleCheck::FillHisto()
{
  int npacket = 0;
  for (packetiter = pkt.begin(); packetiter != pkt.end(); packetiter++)
    {
      npacket++;
      ErrorCounterHisto->SetBinContent(npacket, LENGTHBIN, packetiter->BadLength);
      ErrorCounterHisto->SetBinContent(npacket, DCMCHKSUMBIN, packetiter->DcmCheckSumError);
      ErrorCounterHisto->SetBinContent(npacket, FEMPARITYBIN, packetiter->FEMParityError);
      ErrorCounterHisto->SetBinContent(npacket, FEMCLKBIN, packetiter->FEMClockCounterError);
      ErrorCounterHisto->SetBinContent(npacket, FEMGL1CLKBIN, packetiter->FEMGL1ClockError);
      ErrorCounterHisto->SetBinContent(npacket, FEMEVTSEQBIN, packetiter->FEMEvtSeqError);
      ErrorCounterHisto->SetBinContent(npacket, NPACKETBIN, packetiter->PacketCounter);
      ErrorCounterHisto->SetBinContent(npacket, NGOODPACKETBIN, packetiter->GoodPackets);
      ErrorCounterHisto->SetBinContent(npacket, SUMBYTESBIN, packetiter->SumBytes);
      ErrorCounterHisto->SetBinContent(npacket, DCMFEMPARBIN, packetiter->DcmFEMParityError);
      ErrorCounterHisto->SetBinContent(npacket, GLINKBIN, packetiter->GlinkError);
      ErrorCounterHisto->SetBinContent(npacket, SUBSYSTEMBIN, packetiter->SubsystemError);
    }
  variableHisto->SetBinContent(NUMEVENTBIN, rawdatacheck->RawDataCheckEvts());
  return 0;
}

int
GranuleCheck::process_event(Event *e)
{
  packets_found = 0;
  gl1syncerror = 0;
  gl1syncok = 0;
  for (packetiter = pkt.begin(); packetiter != pkt.end(); packetiter++)
    {
      int isGood = 0;
      DoEveryEvent(e);
      p = e->getPacket(packetiter->PacketId);
      if (p)
        {
          packetiter->PacketCounter++;
          packetLength = p->getLength();
          if (packetLength > 2000)
            {
              packetiter->BadLength++;
              packetiter->LastClock = -1; // do not dare get clk counter from this packet
              delete p;
              rawdatacheck->AddBadPacket(packetiter->PacketId, RawChk::BADLEN);
              continue;
            }
          packetiter->SumBytes += packetLength << 2; // bitshift by 2 is equal to multiply by 4 (but faster)
          if (p->getDebugLength())
            {
              if (DcmCheckSumCheck()) // corrupt data can cause crashes
                {
                  packetiter->LastClock = -1; // do not dare get clk counter from this packet
                  delete p;
                  // we do not add this to the bad packets list we have a loop over all
                  // packets in the RawDataCheck which applies this check to all packtes in an event
                  continue;
                }
            }
          packets_found++; // increment number of found packets for subsystem
          hitformat = p->getHitFormat();
          int itst = 0;
          if ((itst = FEMParityErrorCheck()))
            {
              rawdatacheck->AddBadPacket(packetiter->PacketId, RawChk::FEMParity);
            }
          isGood += itst;
          if ( (itst = DcmFEMParityErrorCheck()) )
            {
              rawdatacheck->AddBadPacket(packetiter->PacketId, RawChk::DcmFEMParity);
            }
          isGood += itst;

          isGood += GlinkCheck();
          SetBeamClock();  // set the Beam Clock Counter for the following clk checks
          if ( (itst = FEMClockCounterCheck()) )// check inter FEM consistency
            {
              rawdatacheck->AddBadPacket(packetiter->PacketId, RawChk::FEMClock);
            }
          isGood += itst;

          if (rawdatacheck->GL1exists())
            {
              if ( (itst = FEMGL1ClockCounterCheck())) // check GL1 - FEM consistency
                {
                  rawdatacheck->AddBadPacket(packetiter->PacketId, RawChk::GL1Clock);
                }
              isGood += itst;
            }

          if ( (itst = FEMEventSequenceCheck()) )
            {
              rawdatacheck->AddBadPacket(packetiter->PacketId, RawChk::FEMEvent);
            }
          isGood += itst;
          if ( (itst = SubsystemCheck(e, SUBSYS_PACKETEXIST)))
            {
              rawdatacheck->AddBadPacket(packetiter->PacketId, itst);
              itst = -1; // just to be consistent with the other return codes
            }
          isGood += itst;
          if (isGood == 0)
            {
              packetiter->GoodPackets++;
            }
          delete p;
        }
      else
        {
          SubsystemCheck(e, SUBSYS_PACKETMISS);
          packetiter->LastClock = -1; // this packet has been dropped from this event
        }
    }
  // Now all packets have been analysed, here come the global checks
  GranuleGL1SynchCheck();
  // number of Granule GL1 sync errors (FATAL!!!)

  rawdatacheck->AddFatalGl1Error(totalgl1syncerror);
  variableHisto->SetBinContent(GL1FATALBIN, totalgl1syncerror);
  variableHisto->SetBinContent(SUBSYSFATALBIN, totalsubsystemerror);
  return 0;
}

int
GranuleCheck::Init(RawDataCheck *raw)
{
  rawdatacheck = raw;
  int iret = Init();
  PacketResetAll();
  CreateHisto();
  return iret;
}

int
GranuleCheck::InitPacketIdHisto()
{
  unsigned int nbin = 0;
  for (packetiter = pkt.begin(); packetiter != pkt.end(); packetiter++)
    {
      nbin++;
      packetHisto->SetBinContent(nbin, packetiter->PacketId);
    }
  return 0;
}

int
GranuleCheck::BeginRun(const int runno)
{
  return InitPacketIdHisto();
}

int
GranuleCheck::PacketResetAll()
{
  nfill = 0;
  for (packetiter = pkt.begin(); packetiter != pkt.end(); packetiter++)
    {
      packetiter->LastClock = -1;
    }
  PacketErrorReset();
  return 0;
}

int
GranuleCheck::PacketErrorReset()
{
  for (packetiter = pkt.begin(); packetiter != pkt.end(); packetiter++)
    {
      packetiter->BadLength = 0;
      packetiter->DcmCheckSumError = 0;
      packetiter->FEMParityError = 0;
      packetiter->FEMClockCounterError = 0;
      packetiter->FEMGL1ClockError = 0;
      packetiter->FEMEvtSeqError = 0;
      packetiter->PacketCounter = 0;
      packetiter->GoodPackets = 0;
      packetiter->SumBytes = 0;
      packetiter->DcmFEMParityError = 0;
      packetiter->GlinkError = 0;
      packetiter->SubsystemError = 0;
    }
  return 0;
}

int
GranuleCheck::DcmCheckSumCheck()
{
  int iret = p->getCheckSumStatus();
  switch (iret)
    {
    case 1:
      return 0;

    case 0:
      if (verbosity > 0)
        {
          cout << "GranuleCheck::DcmCheckSumCheck(): "
               << "Event " << rawdatacheck->EventNumber()
               << " Packet " << packetiter->PacketId
               << " has no dcm chk sum" << endl;
        }
      packetiter->DcmCheckSumError = -1;
      return 0;

    case -1:
      if (verbosity > 0)
        {
          cout << "GranuleCheck::DcmCheckSumCheck(): "
               << "Event " << rawdatacheck->EventNumber()
               << " Packet " << packetiter->PacketId
               << " failed dcm chk sum" << endl;
        }
      packetiter->DcmCheckSumError++;
      break;
    default:
      if (verbosity > 0)
        {
          cout << "GranuleCheck::DcmCheckSumCheck(): "
               << "Event " << rawdatacheck->EventNumber()
               << " Packet " << packetiter->PacketId
               << " has invalid dcm CheckSumStatus: " << iret << endl;
        }
      packetiter->DcmCheckSumError++;
      break;
    }
  return -1;
}


int
GranuleCheck::FEMParityErrorCheck()
{
  int iret = p->iValue(0, "CHECKPARITY");
  switch (iret)
    {
    case 0:
      packetiter->FEMParityError = -1;
      return 0;

    case 1:
      return 0;

    case -1:
      if (verbosity > 0)
        {
          cout << "GranuleCheck::FEMParityErrorCheck(): "
               << "Event " << rawdatacheck->EventNumber()
               << " Packet " << packetiter->PacketId
               << " failed parity check for hitformat " << hitformat << endl;
        }
      packetiter->FEMParityError++;
      break;

    default:
      cout << "GranuleCheck::FEMParityErrorCheck(): "
           << "Event " << rawdatacheck->EventNumber()
           << " Packet " << packetiter->PacketId
           << " has invalid CHECKPARITY status" << endl;
      packetiter->FEMParityError++;
      break;
    }
  return -1;
}

int
GranuleCheck::DcmFEMParityErrorCheck()
{
  if (p->iValue(0, "SUMMARY") & 0x4)
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
GranuleCheck::GlinkCheck()
{
  if (p->iValue(0, "SUMMARY") & 0x2)
    {
      packetiter->GlinkError++;
      return -1;
    }
  return 0;
}

void
GranuleCheck::SetBeamClock()
{
  BeamClock = p->iValue(0, "BCLK");
  return ;
}

int
GranuleCheck::FEMClockCounterCheck()
{
  static unsigned int standard_clock;
  if (packets_found == 1)
    {
      standard_clock = BeamClock;
    }
  else
    {
      if (BeamClock != standard_clock)
        {
          if (verbosity > 0)
            {
              cout << ThisName << ": Packet: " << packetiter->PacketId
                   << " FEMClock Counter Error: clock 0x" << hex << BeamClock
                   << " ref clock: "  << standard_clock << dec << endl;
            }
          packetiter->FEMClockCounterError++;
          return -1;
        }
    }
  return 0;
}

int
GranuleCheck::FEMGL1ClockCounterCheck()
{
  // exclude bad beam clocks (e.g. rollover problem in pc)
  if (BadBeamClock())
    {
      packetiter->LastClock = -1;
      return 0;
    }
  int clock_diff;
  if (packetiter->LastClock >= 0 && rawdatacheck->LastGL1Clock() >= 0)
    {
      int tmp1 = packetiter->LastClock & 0xFF;
      int tmp2 = BeamClock & 0xFF;
      clock_diff = tmp1 - tmp2;
      if (clock_diff < 0)
        {
          clock_diff += 256;
        }
      if (clock_diff != rawdatacheck->GL1ClockDiff())
        {
          if (verbosity > 0)
            {
              cout << ThisName << ": Packet: " << packetiter->PacketId
                   << ": GL1 Clock Counter Error: Granule clock_diff: 0x" << hex << clock_diff
                   << ", GL1 clkdiff: 0x" << rawdatacheck->GL1ClockDiff()
                   << ", diff: 0x" << abs(clock_diff - rawdatacheck->GL1ClockDiff())
                   << ", Gl1 clk: 0x" << rawdatacheck->LastGL1Clock()
		//                      	       << ", lastclock: 0x" << lastclock
		//           		       << ", BeamClock: 0x" << BeamClock
                   << dec << endl;
            }
          gl1syncerror++;
          packetiter->FEMGL1ClockError++;
          // if we set this to the beam clock and the next beam clock is okay, this would
          // just trigger the next gl1 error which would be false, so discard this
          // trigger from the gl1 crosscheck
          packetiter->LastClock = -1;
          return -1;
        }
      else
        {
          gl1syncok++;
        }
    }
  packetiter->LastClock = BeamClock;
  return 0;
}

unsigned int
GranuleCheck::LocalEventCounter()
{
  return p->iValue(0, "EVTNR");
}

unsigned int
GranuleCheck::GlobalEventCounter()
{
  return rawdatacheck->GL1GranuleCounter(GranuleNumber);
}


int
GranuleCheck::FEMEventSequenceCheck()
{
  int local_event_counter = LocalEventCounter();
  if (! rawdatacheck->GL1exists())
    {
      if (local_event_counter != rawdatacheck->EventNumber() + LocalEventNumberOffset())
        {
          if (verbosity > 0)
            {
              cout << "packetiter->PacketId: " << packetiter->PacketId
                   << " local_event_counter: " << local_event_counter
                   << " rawdatacheck->EventNumber() + LocalEventNumberOffset(): "
                   << rawdatacheck->EventNumber() + LocalEventNumberOffset() << endl;
            }
          packetiter->FEMEvtSeqError++;
          return -1;
        }
    }
  else
    {
      if ( (local_event_counter&0xFFFF) != ((int) GlobalEventCounter()))
        {
          if (verbosity > 0)
            {
              cout << ThisName << " <E> FEMEventSequenceCheck: Packet: " << packetiter->PacketId
                   << " local_event_counter: 0x" << hex << local_event_counter << dec
                   << " GlobalEventCounter(" << GranuleNumber << "): 0x"
                   << hex << GlobalEventCounter() << dec << endl;
            }
          packetiter->FEMEvtSeqError++;
          return -1;
        }
    }
  return 0;
}

int
GranuleCheck::GranuleGL1SynchCheck()
{
  if (gl1syncok == 0)
    {
      // at least 10% of all packets have exist in this event
      // before a failing of granule is declared
      if (gl1syncerror > num_all_packets / 10)
        {
          totalgl1syncerror++;
          cout << ", Granule " << ThisName << " out of sync with GL1 for the "
               << totalgl1syncerror << " time, No of Packets in this event: "
               << packets_found << " out of " << pkt.size()
               << " Event: " << rawdatacheck->EventNumber() << endl;
          return -1;
        }
      else
        {
          // no packets could be checked (last beam clock = -1)
          // if granule is out of sync only every other
          // packet can be checked
          // or less than 10% of granule fired
          return 0;
        }

    }
  totalgl1syncerror = 0;
  return 0;
}

int
GranuleCheck::SubsystemCheck(Event *evt, const int iwhat)
{
  packetiter->SubsystemError = -1;
  return 0;
}

int
GranuleCheck::MissingPacketCheck(Event *evt)
{
  if (!rawdatacheck->isGranuleActive(GranuleNumber))
    {
      if (verbosity > 0)
        {
          cout << "Granule " << ThisName << " not active" << endl;
        }
      return 0;
    }
  int evttype = evt->getEvtType();
  switch (evttype)
    {
    case DATAEVENT:
      packetiter->SubsystemError++;
      {
        if (verbosity > 0)
          {
            cout << "Granule " << ThisName << " misses packet " <<  packetiter->PacketId << endl;
          }
        rawdatacheck->MissingPacket();
        rawdatacheck->SetReturnCode(ABORTEVENT);
      }
      return -1;
    default:
      return 0;
    }
}

int
GranuleCheck::AddPacketsToAllPacketList()
{
  int iret = 0;
  for (packetiter = pkt.begin(); packetiter != pkt.end(); packetiter++)
    {
      iret += rawdatacheck->AddToAllPacketList(packetiter->PacketId);
    }
  return iret;
}
