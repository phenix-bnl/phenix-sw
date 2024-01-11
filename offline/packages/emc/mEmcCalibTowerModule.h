#ifndef __MEMCCALIBTOWERMODULE_H__
#define __MEMCCALIBTOWERMODULE_H__
/* Automatically generated.  Do not edit. */
#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) see mEmcCalibTower_().
    This module makes calibrated data (GeV, ns) from the raw data.
    In case of simulation this is done by trivial subtraction of
    pedestal and multiplication with a fixed gain.  When analyzing
    real data it should be replaced with the detailed calibration
    and correction routines used in online.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
    @ingroup staf
*/

class PHCompositeNode;

class mEmcCalibTowerModule: public SubsysReco
{
public:
  mEmcCalibTowerModule();
  virtual ~mEmcCalibTowerModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCCALIBTOWERMODULE_H__*/
