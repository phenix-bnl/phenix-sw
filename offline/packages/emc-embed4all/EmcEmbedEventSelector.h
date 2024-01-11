#ifndef __EMCEMBEDEVENTSELECTOR_H__
#define __EMCEMBEDEVENTSELECTOR_H__

#include <string>
#include "SubsysReco.h"

class Counters;
class SimulationReader;

/** Select events where REAL and SIMU vertices match.
    Match is hereby defined as abs(delta(z)) < zcut.
 */

class EmcEmbedEventSelector : public SubsysReco
{
 public:

  /** @params zlimit : abs(real vertex) must be < zlimit.
      @params deltazlimit : for the real events, satisfying
      the zlimit, we consider only simulated events for which
      abs(real vertex-simu vertex) < deltazlimit.
  */
  EmcEmbedEventSelector(const char* simuinputfile,
			float zlimit = 30, /* cm */
			float deltazlimit = 5, /*cm */
			const char* realTopNode = "REAL",
			const char* simuTopNode = "SIMU",
			size_t maxnumberofiterations = 10,
			float simulatedpoolfractiontouse= 1.0);

  virtual ~EmcEmbedEventSelector();

  int InitRun(PHCompositeNode*);

  int Init(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:

  float fZcut;
  std::string fRealNode;
  Counters* fCounters;
  SimulationReader* fSimulationReader;
};

#endif
