#include "EventLocatev1.h"

#include <cstdlib>

using namespace std;

EventLocatev1::EventLocatev1()
{
  return;
}

void EventLocatev1::Reset()
{
  // we don't want to reset this for each event
  // so just return here
  return;
}

void EventLocatev1::identify(ostream& out) const
{
  out << "identify yourself: I am an EventLocatev1 Object" << endl;
  out << "size: " << evtloc.size() << endl;
  return;
}

int
EventLocatev1::isValid() const
{
  return((evtloc.size()) ? 1:0); // return 1 if map has entries not zero
}

void
EventLocatev1::insert(const int evtseq, const int evtcount)
{
  if (evtcount < 0)
    {
      cout << "invalid negative count " << evtcount
	   << " for event " << evtseq << endl;
      exit(1);
    }
  if (evtloc.find(evtseq) != evtloc.end())
    {
      cout << "duplicate entry for event no " << evtseq
	   << ", already assigned to slot " << evtloc.find(evtseq)->second
	   << " new count : " << evtcount << endl;
      exit(1);
    }
  evtloc[evtseq] = evtcount;
  return;
}

int
EventLocatev1::get_entry( const int evtseq) const
{
  map<int, int>::const_iterator iter = evtloc.find(evtseq);
  if (iter == evtloc.end())
    {
      return -1;
    }
  return iter->second;
}
