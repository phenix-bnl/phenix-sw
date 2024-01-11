#include "SimulationReader.h"

#include <cassert>
#include <cmath>
#include <iostream>
#include <algorithm>
#include <iterator>

#include <frog/FROG.h>
#include <Fun4AllServer.h>
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include <PHNodeIOManager.h>
#include "VertexGetter.h"


std::ostream& operator<<(std::ostream& os, const SimulationReader::Event& e)
{ e.print(os); return os; }

//_____________________________________________________________________________
SimulationReader::SimulationReader(const char* inputfile,
                                   const char* topnode,
                                   float deltazlimit,
                                   size_t maxnumberofiterations,
                                   float simupoolfractiontouse) :
  fInputFile(inputfile),
  fTopNode(topnode),
  fDeltaZlimit(deltazlimit),
  fMaxNumberOfIterations(maxnumberofiterations),
  fPoolFractionToUse(simupoolfractiontouse),
  fCurrentEvent(0),
  fNrewind(0),
  fNused(0),
  fNevents(0),
  fIsValid(false)
{
  FROG fr;
  fInputFile = fr.location(inputfile);
  Fun4AllServer* se = Fun4AllServer::instance();

  fIManager = new PHNodeIOManager(fInputFile.c_str(), PHReadOnly, PHRunTree);
  if (!fIManager->isFunctional())
    {
      throw;
    }
  PHCompositeNode* runnode = se->getNode("RUN", fTopNode.c_str());
  assert(runnode != 0);
  fIManager->read(runnode);
  delete fIManager;
  fIManager = new PHNodeIOManager(fInputFile.c_str(), PHReadOnly);
  fDstNode = se->getNode("DST", fTopNode.c_str());
  assert(fDstNode != 0);
  bool ok = fIManager->read(fDstNode);
  fIsValid=true;
  fEvents.push_back(Event(fCurrentEvent, vertex(), false));
  fIsValid=false;
  assert(ok == true);
}

//_____________________________________________________________________________
SimulationReader::~SimulationReader()
{
  delete fIManager;
}


//_____________________________________________________________________________
bool
SimulationReader::alreadyHave(size_t eventnumber) const
{
  EventVector::const_iterator it = 
    std::find_if(fEvents.begin(),fEvents.end(),eventFinder(fCurrentEvent));

  return ( it != fEvents.end() );
}

//_____________________________________________________________________________
bool
SimulationReader::alreadyUsed(size_t eventnumber) const
{
  if ( alreadyHave(eventnumber) )		 
    {
      const Event& e = fEvents[eventnumber];
      return e.used();
    }
  return false;
}

//_____________________________________________________________________________
SimulationReader::EReturnCode
SimulationReader::findEvent(float realvertex)
{
  size_t ntrials = 0;

  fIsValid = true;

  while ( 
	 fIsValid && 
	 ( alreadyUsed(fCurrentEvent) ||
	   ( fabs(vertex() - realvertex) > fDeltaZlimit ) ) &&
	 ( fNevents == 0 || ntrials <= fNevents ) &&
	 ( !isPoolExhausted() )
	 )
    {
//       std::cout << fCurrentEvent << " "
// 		<< VertexGetter::getVertex(fTopNode.c_str())
// 		<< " ntrials=" << ntrials
// 		<< std::endl;

      ++fCurrentEvent;
      ++ntrials;
      int rv = fIManager->readSpecific(fCurrentEvent, "DST/VtxOut");
      if (!rv)
        {
	  fNevents=fCurrentEvent;
	  std::cout << PHWHERE << "Input file exhausted fNevents=" 
 		    << fNevents << " fNrewing=" << fNrewind
		    << " Rewinding." << std::endl;
	  if ( fNrewind < fMaxNumberOfIterations )
	    {
	      ++fNrewind;
	      fCurrentEvent=0;
	      int rv = fIManager->readSpecific(fCurrentEvent,"DST/VtxOut");
	      assert(rv!=0);
	      continue;
	    }	  
          return invalidate(kMaxIterationReached);
        }

      if (!alreadyHave(fCurrentEvent))
	{
	  // new event, append it to our list.
          fEvents.push_back(Event(fCurrentEvent, vertex(), false));	
	}
    }

  // Did we exhausted all our events ?
  if ( isPoolExhausted() )
    {
      return invalidate(kSimulationPoolExhausted);
    }

  // Did we loop once over all events with no match ?
  if ( fNevents > 0 && ntrials > fNevents )
    {
      return invalidate(kNoMatch);
    }

  // Update our used counters.
  fEvents[fCurrentEvent].used(true);
  ++fNused;
  // And fully read this even
  fIManager->read(fCurrentEvent);
  fIsValid=true;
  return kOK;
}

//_____________________________________________________________________________
bool
SimulationReader::isPoolExhausted() const
{
  if ( fNevents == 0 )
    {
      // Don't know yet the number of events we have.
      // Cannot decide yet.
      return false;
    }
  
  size_t limit = static_cast<size_t>(floor(fPoolFractionToUse*fNevents));

  if ( fNused == limit )
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
void
SimulationReader::print(std::ostream& os, int level) const
{
  os << "SimulationReader::print : Reading from "
     << fInputFile << std::endl
     << "CurrentEvent=" << fCurrentEvent
     << " Nrewind=" << fNrewind;
  if ( fNevents )
    {
      os << " Nevents=" << fNevents;
    }
  os << " Nused=" << fNused << std::endl;

  if ( level > 0 )
    {
      std::copy(fEvents.begin(),fEvents.end(),
		std::ostream_iterator<Event>(os,""));
    }
}

//_____________________________________________________________________________
float
SimulationReader::vertex() const
{
  if ( fIsValid )
    {
      return VertexGetter::getVertex(fTopNode.c_str());
    }
  else
    {
      return sqrt(-1.0);
    }
}
