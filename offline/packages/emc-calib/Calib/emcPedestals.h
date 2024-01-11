#ifndef __EMCPEDESTALS_H__
#define __EMCPEDESTALS_H__

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif

/** (OLD) Stores pedestal values for several FEMs.
    \deprecated
    @ingroup oldemccalib
 */

class emcPedestals : public emcFEMtuple
{

 public:

  virtual ~emcPedestals() {;}

  /// Category of this emcManageable = "Pedestals"
  virtual const char* GetCategory(void) const { return "Pedestals" ; }

};

#endif
