#include "SingleEventFetcher.h"

#include "EventLocate.h"

#include <EventHeader.h>
#include <SyncObject.h>
#include <SvxPixelRawHitList.h>

#include <Fun4AllDstInputManager.h>
#include <Fun4AllServer.h>
#include <Fun4AllUtils.h>

#include <getClass.h>

#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>

#include <cstdlib>
#include <sstream>

using namespace std;

SingleEventFetcher::SingleEventFetcher():
  dstin(NULL),
  topnode(NULL),
  loc(NULL),
  runnumber(0),
  segment(-1),
  verbosity(0)
{}

SingleEventFetcher::~SingleEventFetcher()
{
  delete dstin;
  return;
}

int
SingleEventFetcher::FileOpen(const std::string &name)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  pair<int, int> runseg = Fun4AllUtils::GetRunSegment(name);
  runnumber = runseg.first;
  segment = runseg.second;
  ostringstream topname;
  topname << "TOP_" << segment;
  topnode = se->topNode(topname.str());
  ostringstream dstname;
  dstname << "DSTin_" << segment;
  dstin = new Fun4AllDstInputManager(dstname.str(),"DST",topname.str());
  dstin->fileopen(name);
  fname = name;
  //  inputmgrs[top] = in;
  ostringstream evtlocnodename;
  evtlocnodename << "EventLocation_" << runnumber << "_" << segment;
  loc = findNode::getClass<EventLocate>(topnode,evtlocnodename.str());
  return 0;
}
void
SingleEventFetcher::identify(std::ostream& os) const
{
  os << "segment " << segment << ", filename: " << fname << endl;
  return;
}

SvxPixelRawHitList *
SingleEventFetcher::getpixelhits(const int evtseq)
{
  int evtcnt = loc->get_entry(evtseq);
  if (verbosity > 0)
    {
      dstin->Verbosity(4);
      cout << "reading evt no " << evtseq << ", count: " << evtcnt << endl;
    }
  if (dstin->ReadEvent(evtcnt))
    {
      cout << "reading event " << evtseq << ", count " << evtcnt
	   << " failed" << endl;
    }
  EventHeader *evt = findNode::getClass<EventHeader>(topnode,"EventHeader");
  int eventnumber = evt->get_EvtSequence();
  if (eventnumber != evtseq)
    {
      cout << "Event number mismatch, asked for event " << evtseq
	   << " got event " << eventnumber << " count: " << evtcnt << endl;
      identify();
      evt->identify();
      return NULL;
    }
  SvxPixelRawHitList *pixhits = findNode::getClass<SvxPixelRawHitList>(topnode,"SvxPixelHits");
  SvxPixelRawHitList *pixcopy = new SvxPixelRawHitList(pixhits);
  // cout << "asking for event " << evtseq << " reading row " << evtcnt << endl;
  // sync->identify();
  PHNodeReset reset;
  PHNodeIterator mainIter(topnode);
  if (mainIter.cd("DST"))
    {
      mainIter.forEach(reset);
    }
  else
    {
      cout << "Resetting " << topnode->getName() << "  failed" << endl; 
    }
  return pixcopy;
}
