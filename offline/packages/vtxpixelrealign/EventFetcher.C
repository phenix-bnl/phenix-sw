#include "EventFetcher.h"
#include "EventLocate.h"
#include "SingleEventFetcher.h"

#include <SvxPixelRawHitList.h>

#include <Fun4AllDstInputManager.h>
#include <Fun4AllServer.h>
#include <Fun4AllUtils.h>
#include <getClass.h>

#include <PHCompositeNode.h>

#include <TSystem.h>

#include <cstdlib>
#include <sstream>

using namespace std;

EventFetcher::~EventFetcher()
{
  while(inputs.begin() != inputs.end())
    {
      delete inputs.begin()->second;
      inputs.erase(inputs.begin());
    }
  return;
}


int
EventFetcher::OpenDST(const std::string &name)
{
  if (files.find(name) != files.end())
    {
      cout << "file " << name << " already opened" << endl;
      return -1;
    }
  SingleEventFetcher *evtfetch = new SingleEventFetcher();
  evtfetch->FileOpen(name);
  EventLocate *loc = evtfetch->GetEventLocate();
  int segment = evtfetch->Segment();
  if (inputs.find(segment) != inputs.end())
    {
      cout << "Error: segment " << segment << " already exists" << endl;
      exit(1);
    }
  inputs[segment] = evtfetch;
  pair<map<int, int>::const_iterator, map<int, int>::const_iterator> begend = loc->beginend();
  map<int, int>::const_iterator iter;
  for (iter = begend.first; iter != begend.second; ++iter)
    {
      if (iter->first < 0)
	{
	  continue;
	}
      if (evtseg.find(iter->first) != evtseg.end())
	{
	  cout << "Event " << iter->first << " from segment " << segment
	       << " already exists in segment " << evtseg.find(iter->first)->second << endl;
	  gSystem->Exit(1);
	}
      evtseg[iter->first] = segment;
    }
  loc->identify();
  cout << "entries evtseg: " << evtseg.size() << endl;
  return 0;
}

SvxPixelRawHitList *
EventFetcher::fetchevent(const int evtseq)
{
  map<int, int>::const_iterator iter = evtseg.find(evtseq);
  if (iter == evtseg.end())
    {
      cout << "could not locate event " << evtseq << endl;
      return NULL;
    }
  SingleEventFetcher *snglevt = inputs.find(iter->second)->second;
  //  snglevt->identify();
  SvxPixelRawHitList *obj = snglevt->getpixelhits(evtseq);
  if (! obj)
    {
      cout << "could not find event number " << evtseq << endl;
      cout << "checked segment " << iter->second << endl;	  
    }
  return obj;
}


