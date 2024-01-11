#ifndef __EMCEMBEDVERTEXSELECTOR_H__
#define __EMCEMBEDVERTEXSELECTOR_H__

#include "SubsysReco.h"
#include <string>

class Counters;

/** Select events where REAL and SIMU vertices match.
    Match is hereby defined as both vertices between zmin and zmax.
 */

class EmcEmbedVertexSelector : public SubsysReco
{
 public:
  EmcEmbedVertexSelector(float zmin = -7, 
			 float zmax = +7 /* cm */,
			 const char* realTopNode = "REAL",
			 const char* simuTopNode = "SIMU");

  virtual ~EmcEmbedVertexSelector();

  int Init(PHCompositeNode*);

  int process_event(PHCompositeNode*);

  float fZmin;
  float fZmax;
  std::string fRealNode;
  std::string fSimuNode;
  Counters* fCounters;
};

#endif
