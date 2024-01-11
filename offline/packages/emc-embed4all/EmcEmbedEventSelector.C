#include "EmcEmbedEventSelector.h"

#include <cassert>
#include <cmath>

#include "Counters.h"
//INCLUDECHECKER: Removed this line: #include "Fun4AllServer.h"
//INCLUDECHECKER: Removed this line: #include "Fun4AllInputManager.h"
#include "Fun4AllReturnCodes.h"
#include "PHCompositeNode.h"
#include "VertexGetter.h"
#include "SimulationReader.h"

namespace
{  
  const char* kRealEvents="Real Events";
  const char* kDiscardedRealEvents="Real Events Discarded (because no matching simulated vertex found)";
  const char* kRealEventsInVertexCut="Real Events That Matches the real vertex cut";
  const char* kMatches="Events that matches";
  const char* kNanVertex="NaN real vertices";

//   const char* kTrials="Simulated events tried (1 evt could be tried several times)";
  const char* kSimulationPoolExhausted="Simulation pool exhausted";
  const char* kMaxIterationReached="Max number of allowed iterations reached";
}

//_____________________________________________________________________________
EmcEmbedEventSelector::EmcEmbedEventSelector(const char* simuinputfile,
					     float zlimit, 
					     float deltazlimit,
					     const char* realTopNode,
					     const char* simuTopNode,
					     size_t maxnumberofiterations,
					     float simupoolfractiontouse)
  : SubsysReco("EmcEmbedEventSelector"),
    fZcut(zlimit), 
    fRealNode(realTopNode),
    fCounters(new Counters),
    fSimulationReader(0)
{
  if ( fZcut <= 0 )
    {
      throw;
    }
  
  fSimulationReader = new SimulationReader(simuinputfile,
					   simuTopNode,
					   deltazlimit,
					   maxnumberofiterations,
					   simupoolfractiontouse);
}

//_____________________________________________________________________________
EmcEmbedEventSelector::~EmcEmbedEventSelector()
{
  std::cout << PHWHERE << "dtor summary:" << std::endl;
  fCounters->print();
  delete fCounters;
  fSimulationReader->print();
  std::cout << std::endl;
}

//_____________________________________________________________________________
int
EmcEmbedEventSelector::Init(PHCompositeNode*)
{
  fCounters->add(kRealEvents);
  fCounters->add(kRealEventsInVertexCut);
  fCounters->add(kDiscardedRealEvents);
  fCounters->add(kMatches);
  fCounters->add(kSimulationPoolExhausted);
  fCounters->add(kMaxIterationReached);
  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedEventSelector::InitRun(PHCompositeNode*)
{
  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedEventSelector::process_event(PHCompositeNode*)
{
  fCounters->incr(kRealEvents);

  // First check that real event is within our acceptable limits.
  float rz = VertexGetter::getVertex(fRealNode);

  if ( isnan(rz) || fabs(rz) > fZcut )
    {
      if ( isnan(rz) )
	{
	  fCounters->incr(kNanVertex);
	}
      return ABORTEVENT;
    }

  fCounters->incr(kRealEventsInVertexCut);

  // Then only, we try to find a matching simulated vertex.
  SimulationReader::EReturnCode iret = fSimulationReader->findEvent(rz);

  if ( iret != SimulationReader::kOK )
    {
      // This is the end. Let's see why we end.
      if ( iret == SimulationReader::kNoMatch )
	{
	  // This is not really the end !
	  // No match found, discard this real event 
	  // and try again.
	  std::cout << PHWHERE << " Skipping one real event z=" 
		    << VertexGetter::getVertex(fRealNode) << std::endl;
	  fCounters->incr(kDiscardedRealEvents);
	  return DISCARDEVENT;
	}
      if ( iret == SimulationReader::kSimulationPoolExhausted )
	{
	  // That's an end.
	  std::cout << PHWHERE << " Simulation pool exhausted." << std::endl;
	  fCounters->incr(kSimulationPoolExhausted);
	  return ABORTRUN;
	}
      if ( iret == SimulationReader::kMaxIterationReached )
	{
	  // That's an end as well.
	  std::cout << PHWHERE << " Max # of iterations reached." << std::endl;
	  fCounters->incr(kMaxIterationReached);
	  return ABORTRUN;
	}
      assert(0==1);
    }

  if ( verbosity > 0 )
    {
      float rz = VertexGetter::getVertex(fRealNode);
      float sz = fSimulationReader->vertex();
      std::cout << PHWHERE << "Sim Event " 
		<< fSimulationReader->eventNumber() 
		<< " -> vertices: real=" 
		<< rz << " simu=" << sz
		<< " delta=" << rz-sz << std::endl;
    }
  
  fCounters->incr(kMatches);
  return EVENT_OK;
}
