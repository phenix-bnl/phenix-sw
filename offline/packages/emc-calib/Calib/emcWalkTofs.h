#ifndef __EMCWALKTOFS_H__
#define __EMCWALKTOFS_H__

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif

/** (OLD) Stores walk tof value for several FEMs.
    \deprecated
    @ingroup oldemccalib
 */

class emcWalkTofs : public emcFEMtuple
{

 public:

  /// Destructor
  virtual ~emcWalkTofs() {;}

  /// Category of this emcManageable = "WalkTofs"
  const char* GetCategory(void) const { return "WalkTofs"; }
  
  virtual float GetValue1(int ichannel) const;
  virtual float GetValue2(int ichannel) const;
};

#endif
