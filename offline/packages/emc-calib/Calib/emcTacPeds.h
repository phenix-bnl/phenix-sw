#ifndef __EMCTACPEDS_H__
#define __EMCTACPEDS_H__

#ifndef __EMCGAINGS_H__
#include "emcGains.h"
#endif

/** (OLD) Stores TAC Pedestal Drifts for several FEMs.
    \deprecated
    @ingroup oldemccalib
*/

class emcTacPeds : public emcGains
{
 public:

  /// Category = "TacPeds"
  virtual const char* GetCategory(void) const { return "TacPeds"; } 

} ;

#endif
