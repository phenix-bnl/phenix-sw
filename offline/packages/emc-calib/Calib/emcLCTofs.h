#ifndef __EMCLCTOFS_H__
#define __EMCLCTOFS_H__

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif

/** (OLD) Stores least-count tof values for several FEMs.
    \deprecated
    @ingroup oldemccalib
 */

class emcLCTofs : public emcFEMtuple
{

 public:

  /// Destructor
  virtual ~emcLCTofs() {;}

  /// Category of this emcManageable = "LCTofs"
  const char* GetCategory(void) const { return "LCTofs" ; }

  virtual float GetValue1(int ichannel) const;
  virtual float GetValue2(int ichannel) const;
};

#endif
