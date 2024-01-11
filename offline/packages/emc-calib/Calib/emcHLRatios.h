#ifndef __EMCHLRATIOS_H__
#define __EMCHLRATIOS_H__

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif

/** (OLD) Stores H/L gain ratio values for several FEMs.
    \deprecated
    @ingroup oldemccalib
 */

class emcHLRatios : public emcFEMtuple
{

 public:

  /// dtor. Does nothing.
  virtual ~emcHLRatios() {;}

  /// Category of this emcManageable = "HLRatios"
  const char* GetCategory(void) const { return "HLRatios" ; }

};

#endif
