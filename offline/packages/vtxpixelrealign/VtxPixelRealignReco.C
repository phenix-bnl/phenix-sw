#include "VtxPixelRealignReco.h"
#include "EventFetcher.h"

#include <EventHeader.h>
#include <SvxPixelRawHitList.h>

#include <PdbMapIntMapIntInt.hh>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbBankID.hh>
#include <RunToTime.hh>

#include <PHIODataNode.h>
#include <PHCompositeNode.h>

#include <recoConsts.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <getClass.h>

#include <TSystem.h>

#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

VtxPixelRealignReco::VtxPixelRealignReco(const string &name):
  SubsysReco(name),
  runnumber(0),
  dropevents(-1),
  usedb(0),
  evfetch(new EventFetcher())
{
  eventchipmap[0] = chipmap;
  memset(svxpixelhits, 0, sizeof(svxpixelhits));
  return ;
}

VtxPixelRealignReco::~VtxPixelRealignReco()
{
  delete evfetch;
  return;
}

int
VtxPixelRealignReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int currentrun = rc->get_IntFlag("RUNNUMBER");
  if (usedb)
    {
      fetch(currentrun);
    }
  if (currentrun != runnumber)
    {
      cout << "run number from calibration file/DB " << runnumber
	   << " does not match DST run number " << currentrun
	   << endl;
      gSystem->Exit(1);
    }
  return EVENT_OK;
}

int
VtxPixelRealignReco::process_event(PHCompositeNode *topNode)
{
  EventHeader *evthead = findNode::getClass<EventHeader>(topNode, "EventHeader");

  int evtseq = evthead->get_EvtSequence();
  for (map<int, boost::numeric::interval<int> >::const_iterator iter = dropeventrange.begin(); iter != dropeventrange.end(); ++iter)
    {
      if  (boost::numeric::in(evtseq, iter->second))
	{
	  if (verbosity > 1)
	    {
	      cout << "event " << evtseq << " aborted" << endl;
	    }
	  return ABORTEVENT;
	}
    }
  // in case there are no jumps, the stl map is empty and we
  // do not have to do anything, the SvxPixelHits will not be modified
  if (eventchipmap.empty())
    {
      return EVENT_OK;
    }
  svxpixelhits[0] = findNode::getClass<SvxPixelRawHitList>(topNode, "SvxPixelHits");
  // for keylow < evtseq < keyhigh this picks keylow
  // and key for evtseq == key
  map<int, map<int, int> >::const_iterator iter = eventchipmap.upper_bound(evtseq);
  --iter;
  // cout << "evtseq: " << evtseq << " upper_bound --: " << iter->first << endl;
  // cout << " pixel misalignment size: " << iter->second.size() << endl;
  map<int, int>::const_iterator chipiter;
  // svxpixelhits[0]->identify();
  for (chipiter = iter->second.begin(); chipiter != iter->second.end(); ++chipiter)
    {
      int evtseqcnt =  evtseq;
      if (verbosity > 0)
	{
	  cout << "chip " << chipiter->first << " jumped by " << fabs(chipiter->second) << " for event " << evtseq << endl;
	}
      for (int i = 1; i < 4; i++)
	{
	  evtseqcnt++;
	  if (evtseqcnt > 0)
	    {
	      if (! svxpixelhits[i] && i == abs(chipiter->second))
		{
		  if (verbosity > 0)
		    {
		      cout << "loading for skip " << i
			   << "fetching event " << evtseqcnt << endl;
		    }
		  svxpixelhits[i] = evfetch->fetchevent(evtseqcnt);
		  //cout << "loaded for skip " << i << endl;
		  //	      svxpixelhits[i]->identify();
		  if (! svxpixelhits[i])
		    {
		      cout << "Event " << evtseqcnt << " returned NULL" << endl;
		      return ABORTEVENT;
		    }
		}
	    }
	}
      for (int i = 0; i < 4; i++)
	{
	  if (svxpixelhits[i])
	    {
	      //          svxpixelhits[i]->identify();
	    }
	}
      //      cout << "before reset" << endl;
      // svxpixelhits[0]->Print(chipiter->first);
      svxpixelhits[0]->reset_chip(chipiter->first);
      //      cout << "after reset" << endl;
      //svxpixelhits[0]->Print(chipiter->first);
      if (fabs(chipiter->second) > 3)
	{
	  cout << "bad event jump: " << abs(chipiter->second)
	       << " for chip " << chipiter->first
	       << endl;
	  gSystem->Exit(1);
	}
      std::vector<short> otherhits = svxpixelhits[abs(chipiter->second)]->get_hits(chipiter->first);
      //      cout << "pixelhits[" << abs(chipiter->second) << "]" << endl;
      //svxpixelhits[abs(chipiter->second)]->identify();
      if (! otherhits.empty())
	{
	  //	  cout << "Adding chip " << chipiter->first << ", size " << otherhits.size() << endl;
	  svxpixelhits[0]->add_chipHits(chipiter->first, &otherhits);
	}
      //      cout << "after insert:" << endl;
      //svxpixelhits[0]->Print(chipiter->first);
      //      cout << "after replacing all" << endl;
    }
  // svxpixelhits[0]->identify();
  return EVENT_OK;
}

int
VtxPixelRealignReco::End(PHCompositeNode *topNode)
{
  // std::map<int, std::vector<std::pair<int,int> > >::const_iterator iter;
  // for (iter = eventchipmap.begin(); iter != eventchipmap.end(); iter++)
  //   {
  //     cout << "event: " << iter->first << " vsize: " << iter->second.size() << endl;
  //   }
  return 0;
}

int
VtxPixelRealignReco::OpenDST(const std::string &name)
{
  int iret = evfetch->OpenDST(name);
  return iret;
}

int
VtxPixelRealignReco::ResetEvent(PHCompositeNode *topNode)
{
  // do not reset svxpixelhits[0] which is on the node tree
  for (int i = 1; i < 4; i++)
    {
      delete svxpixelhits[i];
      svxpixelhits[i] = NULL;
    }
  return 0;
}

void
VtxPixelRealignReco::insertmap(const int eventno, const int chipno, const int jump)
{
  if (jump == 0)
    {
      chipmap.erase(chipno);
    }
  else
    {
      chipmap[chipno] = jump;
    }
  eventchipmap[eventno] = chipmap;
}

void
VtxPixelRealignReco::PrintMap() const
{
  std::map<int, std::map<int, int> >::const_iterator iter;
  map<int, int>::const_iterator chipiter;
  for (iter = eventchipmap.begin(); iter != eventchipmap.end(); ++iter)
    {
      cout << "event " << iter->first << endl;
      for  (chipiter = iter->second.begin(); chipiter != iter->second.end(); ++chipiter)
	{
	  cout << "chip " << chipiter->first << ", jump: " << chipiter->second << endl;
	}
    }
  return;
}

void
VtxPixelRealignReco::ReadFile(const string &fname)
{
  ifstream input;
  input.open(fname.c_str(), ifstream::in);
  int event = 0;
  int chip = 0;
  int idummy = 0;
  int jump = 10;
  while(!input.eof())
    {
      input >> runnumber;
      input >> chip;
      input >> event;
      input >> idummy;
      input >> jump;
      insertmap(event, chip, jump);
      if (verbosity > 0)
	{
	  cout << "runnumber: " << runnumber << ", event: " << event
	       << ", chip: " << chip << ", jump: " << jump
	       << endl;
	}
      if (dropevents >= 0)
	{
	  boost::numeric::interval<int> newinterval;
	  try
	    {
	      newinterval.assign(event, event + dropevents);
	    }
	  catch (exception& e)
	    {
	      cout << "Exception thrown: " << e.what() << endl;
	      cout << "for interval[" << event << "," << event + dropevents << "]" << endl;
	      cout << "exiting" << endl;
	      exit(1);
	    }
	  if (dropeventrange.find(event) == dropeventrange.end())
	    {
	      dropeventrange[event] = newinterval;
	    }
	}
    }
  input.close();
  return;
}

int
VtxPixelRealignReco::fetch(const int run)
{
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      cout << PHWHERE << "Aborting ... Database not readable" << endl;
      application->abort();
      return -1;
    }

  //  Make a bank ID
  PdbBankID bankID(0);
  string tablename("svxpixelrealign");
  PdbCalBank *Bank = bankManager->fetchBank("PdbMapIntMapIntIntBank",
					    bankID,
					    tablename.c_str(),
					    run);
  if (Bank)
    {
      PdbMapIntMapIntInt *mapbank = (PdbMapIntMapIntInt *) & (Bank->getEntry(0));
      runnumber = mapbank->get_runnumber();
      eventchipmap = mapbank->get_map();
      delete Bank;
    }
  else
    {
      cout << PHWHERE << "Failed to get vtx pixel event realignment info from DB" << endl;
      return -1;
    }
  return 0;
}

void
VtxPixelRealignReco::print() const
{
  if (eventchipmap.empty())
    {
      cout << "No Entries in Map" << endl;
      return;
    }
  else
    {
      cout << "Map Size = " << eventchipmap.size() << endl;
      map<int, map <int, int> >::const_iterator eviter;
      map <int, int>::const_iterator chipiter;
      for (eviter = eventchipmap.begin(); eviter !=   eventchipmap.end(); ++eviter)
	{
	  cout << "Jump Event number: " << eviter->first << endl;
	  for (chipiter = eviter->second.begin(); chipiter != eviter->second.end(); ++chipiter)
	    {
	      cout << "chip " << chipiter->first << " jump: " << chipiter->second << endl;
	    }
	}
    }
  return;
}
