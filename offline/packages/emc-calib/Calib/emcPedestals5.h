#ifndef __EMCPEDESTALS5_H__
#define __EMCPEDESTALS5_H__

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif

/** (OLD) Store pedestal values for several FEMs.
    \deprecated
    @ingroup oldemccalib
 */

class emcPedestals5 : public emcFEMtuple
{

 public:

  virtual ~emcPedestals5() {;}

  /// Category of this emcManageable = "Pedestals5"
  virtual const char* GetCategory(void) const { return "Pedestals5" ; }

};

#endif
