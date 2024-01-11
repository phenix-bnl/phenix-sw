#include "EmcEmbedVertexSelector.h"

#include <cassert>

#include "Counters.h"
//INCLUDECHECKER: Removed this line: #include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "PHCompositeNode.h"
#include "VertexGetter.h"

namespace
{
  const char* kEvents="Events";
  const char* kSimEventsInVertexCut="Sim Events In VertexCut";
  const char* kRealEventsInVertexCut="Real Events In VertexCut";
  const char* kMatches="Events that matches";
}

//_____________________________________________________________________________
EmcEmbedVertexSelector::EmcEmbedVertexSelector(float zmin, 
					       float zmax,
					       const char* realTopNode,
					       const char* simuTopNode)
  : SubsysReco("EmcEmbedVertexSelector"),
    fZmin(zmin), 
    fZmax(zmax),
    fRealNode(realTopNode),
    fSimuNode(simuTopNode),
    fCounters(new Counters)
{
}

//_____________________________________________________________________________
EmcEmbedVertexSelector::~EmcEmbedVertexSelector()
{
  std::cout << PHWHERE << std::endl;
  fCounters->print();
  delete fCounters;
}

//_____________________________________________________________________________
int
EmcEmbedVertexSelector::Init(PHCompositeNode*)
{
  fCounters->add(kEvents);
  fCounters->add(kSimEventsInVertexCut);
  fCounters->add(kRealEventsInVertexCut);
  fCounters->add(kMatches);

  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedVertexSelector::process_event(PHCompositeNode*)
{
  float rz = VertexGetter::getVertex(fRealNode);
  float sz = VertexGetter::getVertex(fSimuNode);

  fCounters->incr(kEvents);

  int c = 0;

  if ( rz >= fZmin && rz <= fZmax )
    {
      fCounters->incr(kRealEventsInVertexCut);
      ++c;
    }

  if ( sz >= fZmin && sz <= fZmax )
    {
      fCounters->incr(kSimEventsInVertexCut);
      ++c;
    }

  if ( c == 2 )
    {
      fCounters->incr(kMatches);
    }
  else
    {
      return ABORTEVENT;
    }
 
  return EVENT_OK;
}
