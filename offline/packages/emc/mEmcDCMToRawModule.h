#ifndef __MEMCDCMTORAWMODULE_H__
#define __MEMCDCMTORAWMODULE_H__
/* Automatically generated.  Do not edit. */
#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>
/** (STAF) see mEmcDCMToRaw_().

This module converts the data in DCM format into a generic raw
data format that serves as input for the reconstruction.
Detailed Documentation:
\URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
@author Gabor David \URL{mailto:david@bnl.gov}
@version 1.0
@ingroup staf
*/

class PHCompositeNode;

class mEmcDCMToRawModule: public SubsysReco {
public:
  mEmcDCMToRawModule(): SubsysReco("mEmcDCMToRawModule"){}
  virtual ~mEmcDCMToRawModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCDCMTORAWMODULE_H__*/
