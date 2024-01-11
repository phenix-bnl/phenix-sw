#ifndef __mEmcGeaMakeCalibTower_h__
#define __mEmcGeaMakeCalibTower_h__

#include <SubsysReco.h>

/** (STAF) This module makes calibrated data (GeV, ns) from the raw data,
 *  for simulation only. 
 *  This is done by trivial subtraction of pedestal and multiplication 
 *  with a fixed gain.
 *
 *  It converts the dEmcRawData STAF table into emcTowerContainer object.
 *  Inputs (must be present somwhere under top)
 *  - dEmcGeometry STAF table
 *  - dEmcRawData STAF table
 *  - dEmcEvent STAF table (to be deprecated)
 *  - mEmcGeometryModule TObject (needed to get tower global positions)
 *  - emcTowerContainer object (will be resetted prior to work)
 *  - VtxOut object (needed to get flash time)
 *  
 *  Output : filled emcTowerContainer
 * @ingroup staf
 */

class mEmcGeaMakeCalibTower : public SubsysReco
{
public:
  mEmcGeaMakeCalibTower();
  virtual ~mEmcGeaMakeCalibTower();

  int process_event(PHCompositeNode*);
};
#endif
