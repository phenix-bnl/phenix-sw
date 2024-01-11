#ifndef  __EMCTOFT0S_H__
#define  __EMCTOFT0S_H__

#ifndef __EMCGAINS_H__
#include "emcGains.h"
#endif


/** (OLD) Stores T0 drift values for several FEMs.
    \deprecated
    @ingroup oldemccalib
 */

class emcTofT0s : public emcGains
{
 public:

  /// Category = "TofT0s"
  virtual const char* GetCategory(void) const { return "TofT0Bs"; } 

} ;


#endif   

