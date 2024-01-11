#ifndef __MEMCGEAPARAMSMODULE_H__
#define __MEMCGEAPARAMSMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) see mEmcGeaParams_().
    This module uses the emcpar GEANT parameter files (data stored
    with GFDETU in PISA) to reconstruct the geometry as it was defined
    in PISA, and, equally important, to fetch different tracking
    thresholds (needed to "calibrate" the raw data in the sampling
    calorimeter) and parameters needed to interpret the bit-packed
    PISA output.  Finally, it contains info on the way the simulation
    was done (energy deposit, Cherenkov photon generation in PbGl,
    Fast Monte Carlo, etc.)
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
    @ingroup staf
*/

class PHCompositeNode;

class mEmcGeaParamsModule: public SubsysReco {
public:
  mEmcGeaParamsModule();
  virtual ~mEmcGeaParamsModule(){}
  int process_event(PHCompositeNode *);

protected:
  bool wascalled;
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCGEAPARAMSMODULE_H__*/
