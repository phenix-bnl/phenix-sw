#include "RawDataCheck.h"
#include "RawDataCheckDefs.h"
#include "GranuleCheck.h"
#include "RawDataCheckDBodbc.h"
#include <RunNumberRanges.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>

#include <Event.h>
#include <EventTypes.h>
#include <packet_gl1.h>

#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

using namespace std;

RawDataCheck *RawDataCheck::__instance = NULL;

RawDataCheck *RawDataCheck::instance()
{
  if (__instance)
    {
      return __instance;
    }
  __instance = new RawDataCheck();
  return __instance;
}

RawDataCheck::RawDataCheck(const string &name): SubsysReco(name)
{
  baddb = new RawDataCheckDBodbc();
  updatedb = 1; // default is update the DB
// if someone just  makes an instance and adds bad events
// the cvs tag has to be set in the ctor
  cvstag = "NotSet"; 
  return ;
}

RawDataCheck::~RawDataCheck()
{
  if (baddb)
    {
      delete baddb;
    }
  while(Granule.begin() != Granule.end())
    {
      delete Granule.back();
      Granule.pop_back();
    }
  return;
}

int RawDataCheck::Init(PHCompositeNode *topNode)
{
  last_gl1_clock = -1;
  gl1exists = 0;
  eventnumber = -9999;
  runnumber = 0;
  gl1clockdiff = -1;
  gl1_clock_counter = -1;
  fatalgl1error = 0;
  memset(gl1_granule_counter, 0, sizeof(gl1_granule_counter));
  memset(granule_active,0,sizeof(granule_active));
  rawdatacheckevts = 0;
  abortedevents = 0;
  dcmchksumfail.clear();
  multieventbuffered = 0;
  clkdiff_to_previous_event = 0; // 0 is impossible
  runhasgl1data = 0;
  hasmissingpackets = 0;
  searchuncheckedpackets = 0;
  return 0;
}

int RawDataCheck::Reset(PHCompositeNode *topNode)
{
  int iret = 0;
  Init(topNode);
  vector<GranuleCheck *>::iterator graniter;
  for (graniter = Granule.begin(); graniter != Granule.end(); graniter++)
    {
      iret += (*graniter)->Reset();
    }
  fatalgl1error = 0;
  return iret;
}

int RawDataCheck::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int iret = 0;
  Init(topNode);
  runnumber = rc->get_IntFlag("RUNNUMBER");
  vector<GranuleCheck *>::iterator graniter;
  for (graniter = Granule.begin(); graniter != Granule.end(); graniter++)
    {
      iret += (*graniter)->BeginRun(runnumber);
    }
  // set the cvs tag
  if (rc->FlagExist("CVSTAG"))
    {
      cvstag = rc->get_CharFlag("CVSTAG");
    }
  else
    {
      cout << PHWHERE << "No recoConst CharFlag with name CVSTAG found, using NotSet for Bad Event DB entry" << endl;
      cvstag = "NotSet";
    }
  baddb->TableName(cvstag);
  return iret;
}

int RawDataCheck::registerGranule(GranuleCheck *granule)
{
  int iret = granule->Init(this);
  if (iret)
    {
      cout << PHWHERE << " Error initializing granule "
	   << granule->Name() << ", return code: " << iret << endl;
      return iret;
    }
  Granule.push_back(granule);
  granule->AddPacketsToAllPacketList();
  return 0;
}

int RawDataCheck::process_event(PHCompositeNode *topNode)
{
  returncode = EVENT_OK;
  Event *e = findNode::getClass<Event>(topNode, "PRDF");
  if (!e)
    {
      cout << PHWHERE << "Abort Event: NULL Event Pointer" << endl;
      return ABORTEVENT;
    }
  rawdatacheckevts++; // analysed events
  eventnumber = e->getEvtSequence();
  if (e->getErrorCode())
    {
      if (verbosity > 0)
        {
          cout << "Received event " << eventnumber
	       << " with Error code : " << e->getErrorCode() << endl;
        }
      abortedevents++;
      AddToList(e, "EVENTERROR");
      return ABORTEVENT;
    }
  if (e->getEvtType() != DATAEVENT) // check only data events
    {
      return EVENT_OK;
    }
  // check dcm check sums for all packets, if a bit error hits the
  // packet id one get random packet numbers which might be outside
  // of the subsystem packet id's
  if (DcmCheckSumCheck(e))
    {
      if (verbosity > 0)
        {
          cout << PHWHERE << " Event failed DcmCheckSumCheck, returning abort"
	       << endl;
        }
    }
  if (runnumber > BEGIN_OF_RUN10 && runnumber < BEGIN_OF_RUN11_WITH_VTX)
    {
      int framestat = FrameStatusCheck(e);
      if (framestat)
	{
	  ostringstream err_reason;
	  err_reason << "FRAMESTATUS 0x" << hex << framestat << dec;
	  //      cout << "Event with bad framestatus: 0x" << framestat << endl;
	  AddToList(e, err_reason.str());
	  abortedevents++;
	  return ABORTEVENT;
      }
  }
  vector<GranuleCheck *>::iterator graniter;
  // need to declare pkt14009 here because goto statement in GL1 check
  // creates compiler warning about crossing init of pkt14009
  Packet *pkt14009;
  // discard events which are corrupt
  // Test for GL1 data and fill global variables
  Packet *p = e->getPacket(14001);
  if (!p)
    {
      gl1exists = 0;
      if (runhasgl1data) // gl1 should always be there if this run contains gl1 data
        {
          last_gl1_clock = -1;
          returncode = ABORTEVENT;
          goto skipevent;
        }
    }
  else
    {
      runhasgl1data = 1;
      gl1exists = 1;
      gl1_clock_counter = (p->iValue(0, BEAMCTR0) & 0xFFFF); // BEAMCTR0 defined in packet_gl1.h
      for (int k = 0;k < 32;k++)
        {
          gl1_granule_counter[k] = p->iValue(k, GRANCTR); // GRANCTR defined in packet_gl1.h
	  if (gl1_granule_counter[k] > 0)
	    {
	      if (!granule_active[k])
		{
		  granule_active[k] = 1;
		}
	    }
        }
      delete p;

      if (last_gl1_clock >= 0)
        {
          int tmp1 = last_gl1_clock & 0xFF;    // use only lower 2 bytes like FEM ctrs
          int tmp2 = gl1_clock_counter & 0xFF; // use only lower 2 bytes like FEM ctrs
          gl1clockdiff = tmp1 - tmp2;
          if (gl1clockdiff < 0)
            {
              gl1clockdiff += 256;
            }

        }
    }
  // test if event is multi event buffered
  clkdiff_to_previous_event = 0;
  multieventbuffered = 0;
  pkt14009 = e->getPacket(14009);
  if (pkt14009)
    {
      unsigned int prevevt = 0;
      int FoundPreviousEvent = 0;
      int parvect = pkt14009->iValue(0, "PARVECT");
      unsigned int currevt = pkt14009->iValue(0, "EVCLOCK");
      for (int i = 1; i < 4; i++) // there are 4 events kept
        {
          // check if the partition vector is identical (previous evt can be
          // from another partition - we do not want to compare to that one)
          if (parvect == pkt14009->iValue(i, "PARVECT"))
            {
              prevevt = pkt14009->iValue(i, "EVCLOCK");
              FoundPreviousEvent = 1;
              break;
            }
        }
      if (FoundPreviousEvent)
        {
          if (currevt >= prevevt)
            {
              clkdiff_to_previous_event = currevt - prevevt;
            }
          else
            {
              clkdiff_to_previous_event = currevt + (0xFFFFFFFF - prevevt) + 1;
            }
          if (clkdiff_to_previous_event <= 1824)
            {
              multieventbuffered = 1;
            }
        }
      delete pkt14009;
    }

  for (graniter = Granule.begin(); graniter != Granule.end(); graniter++)
    {
      (*graniter)->process_event(e);
    }

  last_gl1_clock = gl1_clock_counter;
  if (searchuncheckedpackets)
    {
      SearchUnchecked(e);
    }

 skipevent:
  if (!badpackets.empty())
    {
      if (verbosity)
        {
          cout << "Bad Packet found : " << endl;
          set<unsigned int>::iterator piter = badpackets.begin();
          for (piter = badpackets.begin(); piter != badpackets.end(); piter++)
            {
              cout << "Packet: " << *piter << endl;
            }
        }
    }
  if (fatalgl1error)
    {
      cout << "Aborting Event, Granule out of sync with GL1" << endl;
      abortedevents++;
      AddToList(e, "GL1SYNC");
      return ABORTEVENT;
    }
  if (fatalgl1error > 100)
    {
      cout << "Aborting Run: 100 consecutive events with Granule out of sync with GL1" << endl;
      return ABORTRUN;
    }
  if (returncode == ABORTEVENT)
    {
      if (!gl1exists)
        {
          AddToList(e, "GL1MISSING");
        }
      else
        {
	  if (hasmissingpackets)
	    {
              AddToList(e, "MISSINGPACKET");
	    }
	  else
	    {
              AddToList(e, "DCMCHECKSUM");
	    }
        }
      abortedevents++;
    }
  return returncode;
}

int RawDataCheck::ResetEvent(PHCompositeNode *topNode)
{
  vector<GranuleCheck *>::iterator graniter;
  for (graniter = Granule.begin(); graniter != Granule.end(); graniter++)
    {
      (*graniter)->ResetEvent();
    }
  badpackets.clear();
  fatalgl1error = 0;
  hasmissingpackets = 0;
  return 0;
}

void RawDataCheck::identify(ostream& out) const
{
  out << "RawDataCheck Object" << endl;
  out << "Currently registered Granule Monitors:" << endl;
  vector<GranuleCheck *>::const_iterator graniter;
  for (graniter = Granule.begin(); graniter != Granule.end(); graniter++)

    {
      (*graniter)->identify(out);
    }
  list<BadEvent>::const_iterator biter;
  if (baddb)
    {
      out << "identify Odbc DB:" << endl;
      baddb->identify(out);
    }
  out << "List of Bad Events:" << endl;
  for (biter = badlist.begin(); biter != badlist.end(); biter++)
    {
      biter->identify(out);
    }
  return ;
}

void RawDataCheck::AddKnownBadPacket(const int ibad)
{
  set<unsigned int>::iterator piter = defaultbadpackets.find(ibad);
  if (piter == defaultbadpackets.end())
    {
      defaultbadpackets.insert(ibad);
    }
  else
    {
      cout << "AddKnownBadPacket: Paket " << ibad
	   << " allready added" << endl;
    }
  return ;
}

void 
RawDataCheck::AddBadPacket(const unsigned int ibad, const unsigned int status)
{
  unsigned int badpkt = status << 16;
  badpkt += ibad;
  set<unsigned int>::iterator piter = defaultbadpackets.find(badpkt);
  if (piter != defaultbadpackets.end())
    {
      return ;
    }
  badpackets.insert(badpkt);
  return ;
}

int RawDataCheck::isBadPacket(const unsigned int ibad) const
{
  set<unsigned int>::const_iterator piter = badpackets.find(ibad);
  if (piter != badpackets.end())
    {
      return 1;
    }
  piter = defaultbadpackets.find(ibad);
  if (piter != defaultbadpackets.end())
    {
      return 1;
    }
  return 0;
}

void RawDataCheck::PrintBadPackets() const
{
  set<unsigned int>::const_iterator piter;
  cout << "Known Bad Packets:" << endl;
  for (piter = defaultbadpackets.begin(); piter != defaultbadpackets.end(); piter++)
    {
      cout << *piter << endl;
    }
  cout << "Bad Packets for this Event:" << endl;
  for (piter = badpackets.begin(); piter != badpackets.end(); piter++)
    {
      cout << *piter << endl;
    }
  return ;
}


int RawDataCheck::RemoveKnownPacket(const int ibad)
{
  set<unsigned int>::iterator piter;
  cout << "Before remove:" << endl;
  for (piter = defaultbadpackets.begin(); piter != defaultbadpackets.end(); piter++)
    {
      cout << "Def Packet: " << *piter << endl;
    }
  piter = defaultbadpackets.find(ibad);
  if (piter != defaultbadpackets.end())
    {
      defaultbadpackets.erase(piter);
    }
  else
    {
      cout << "Packet " << ibad << " not in bad packet list" << endl;
    }
  cout << "After remove:" << endl;
  for (piter = defaultbadpackets.begin(); piter != defaultbadpackets.end(); piter++)
    {
      cout << "Def Packet: " << *piter << endl;
    }
  return 0;
}

const set<unsigned int> *
RawDataCheck::GetBadPacketList(const char *name) const
{
  if (!strcmp(name, "KNOWN"))
    {
      return &defaultbadpackets;
    }
  else
    {
      return &badpackets;
    }
}

int
RawDataCheck::EndRun(const int runnumber)
{
  cout << "RawDataCheck End of Run Summary:" << endl;
  cout << "Checked Events: " << rawdatacheckevts << endl;
  cout << "Events aborted: " << abortedevents << endl;
  map<int, unsigned int>::const_iterator iter;
  for (iter = dcmchksumfail.begin(); iter != dcmchksumfail.end(); iter++)
    {
      cout << "Packet " << iter->first
	   << " #Dcm Check Sum Errors: " << iter->second
	   << endl;
    }
  // fill the error histogram
  vector<GranuleCheck *>::iterator graniter;
  for (graniter = Granule.begin(); graniter != Granule.end(); graniter++)
    {
      (*graniter)->FillHisto();
      (*graniter)->EndRun(runnumber); // granule end of run summary
    }
  // The data base update is done at the end of a run, just in
  // case we ever run more than one run and the second run crashes
  // we still want to have the bad event info from the first run
  // in the DB
  AddToDB();
  badlist.clear();
  return 0;
}

void
RawDataCheck::Verbosity(const int ival)
{
  if (baddb)
    {
      baddb->Verbosity(ival);
    }
  SubsysReco::Verbosity(ival);
  return ;
}

void
RawDataCheck::AddDcmChkSumFail(const int packetid)
{
  map<int, unsigned int>::iterator iter = dcmchksumfail.find(packetid);
  unsigned int cnt = 0;
  if (iter != dcmchksumfail.end())
    {
      cnt = iter->second;
    }
  cnt++;
  dcmchksumfail[packetid] = cnt;
  return ;
}

int
RawDataCheck::DcmCheckSumCheck(Event *e)
{
  int nw = e->getPacketList(plist, 10000);
  int iret = 0;
  if (nw >= 10000)
    {
      cout << PHWHERE << " Packet list too small, aborting Event" << endl;
      for (int i = 0; i < 10000; i++)
        {
          delete plist[i];
        }
      SetReturnCode(ABORTEVENT);
      return ABORTEVENT;
    }
  for (int i = 0; i < nw; i++)
    {
      int check = plist[i]->getCheckSumStatus();
      switch (check)
        {
        case 1:
          break;

        case 0:
          if (verbosity > 1)
            {
              cout << "RawDataCheck::DcmCheckSumCheck(): "
		   << "Event " << EventNumber()
		   << " Packet " << plist[i]->getIdentifier()
		   << " has no dcm chk sum" << endl;
            }
          break;

        case - 1:
          if (verbosity > 0)
            {
              cout << "RawDataCheck::DcmCheckSumCheck(): "
		   << "Event " << EventNumber()
		   << " Packet " << plist[i]->getIdentifier()
		   << " failed dcm chk sum" << endl;
            }
          AddBadPacket(plist[i]->getIdentifier(),RawChk::DcmCheckSum);
          AddDcmChkSumFail(plist[i]->getIdentifier());
          SetReturnCode(ABORTEVENT);
          iret = ABORTEVENT;
          break;
        default:
          cout << "RawDataCheck::DcmCheckSumCheck(): "
	       << "Event " << EventNumber()
	       << " Packet " << plist[i]->getIdentifier()
	       << " has invalid dcm CheckSumStatus: " << check << endl;
          break;
        }
      delete plist[i];
    }
  return iret;
}

int
RawDataCheck::AddToList(Event *evt, const string &str)
{
  if (!updatedb)
    {
      return 0;
    }
  Fun4AllServer *se = Fun4AllServer::instance();
  BadEvent newbad;

  newbad.Run(runnumber);
  newbad.Event(evt->getEvtSequence());
  newbad.SegmentId(se->SegmentNumber());
  newbad.Reason(str);
  newbad.CvsTag(cvstag);
  // try to get the triggers from the gl1 packet
  Packet *p = evt->getPacket(14001);
  if (p)
    {
      newbad.TrigRaw(p->iValue(0, RAWTRIG));
      newbad.TrigLive(p->iValue(0, LIVETRIG));
      newbad.TrigScaled(p->iValue(0, SCALEDTRIG));
      newbad.Crossing(p->iValue(0, CROSSCTR));
      delete p;
    }
  else
    {
      // if gl1 packet is missing we can still get the scaled trigger from
      // the event header
      newbad.TrigScaled(evt->getTagWord(0));
    }
  unsigned int diff_to_prev = 0;
  p = evt->getPacket(14009);
  if (p)
    {
      unsigned int prevevt = 0;
      int FoundPreviousEvent = 0;
      int parvect = p->iValue(0, "PARVECT");
      unsigned int currevt = p->iValue(0, "EVCLOCK");
      for (int i = 1; i < 4; i++) // there are 4 events kept
        {
          // check if the partition vector is identical (previous evt can be
          // from another partition - we do not want to compare to that one)
          if (parvect == p->iValue(i, "PARVECT"))
            {
              prevevt = p->iValue(i, "EVCLOCK");
              FoundPreviousEvent = 1;
              break;
            }
        }
      if (FoundPreviousEvent)
        {
          if (currevt >= prevevt)
            {
              diff_to_prev = currevt - prevevt;
            }
          else
            {
              diff_to_prev = currevt + (0xFFFFFFFF - prevevt) + 1;
            }
        }
      delete p;
    }
  newbad.TicksToLastEvent(diff_to_prev);
  if (verbosity > 0)
    {
      newbad.identify();
    }
  badlist.push_back(newbad);
  return 0;
}

int
RawDataCheck::AddToDB()
{
  if (!updatedb || badlist.empty())
    {
      return 0;
    }
  baddb->AddBadEvents(&badlist);
  WriteFile();
  return 0;
}

void
RawDataCheck::WriteFile()
{
  if (savefile.empty())
    {
      return;
    }
  ofstream out(savefile.c_str());
  list<BadEvent>::const_iterator biter;
  for (biter = badlist.begin(); biter != badlist.end(); biter++)
    {
      biter->identify(out);
    }
  out.close();
  return;
}

int
RawDataCheck::FrameStatusCheck(Event *evt)
{
  int status = evt->getFrameEntry("FRAMESTATUS", 0, 0);
  return status;
}

int
RawDataCheck::AddToAllPacketList(const int pktid)
{
  if (allpackets.find(pktid) != allpackets.end())
    {
      cout << "duplicate packet " << pktid << endl;
      return -1;
    }
  allpackets.insert(pktid);
  return 0;
}

void
RawDataCheck::Print(const std::string &what) const
{
  if (what == "ALLPACKETS" || what == "ALL")
    {
      set<int>::const_iterator iter;
      for (iter = allpackets.begin(); iter != allpackets.end(); iter++)
	{
	  cout << "packet: " << *iter << endl;
	}
    }
  return;
}

int
RawDataCheck::SearchUnchecked(Event *evt)
{
  cout << "searching unchecked evts" << endl;
  int nw = evt->getPacketList(plist, 10000);
  int iret = 0;
  for (int i = 0; i < nw; i++)
    {
      if (allpackets.find(plist[i]->getIdentifier()) == allpackets.end())
	{
	  cout << "Check for packet " << plist[i]->getIdentifier()
	       << " missing" << endl;
	  iret = -1;
	}
      delete plist[i];
    }
  return iret;
}
