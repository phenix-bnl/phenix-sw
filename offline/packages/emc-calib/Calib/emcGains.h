#ifndef  __EMCGAINS_H__
#define  __EMCGAINS_H__

class PHTimeStamp ;

#ifndef __EMCFEMTUPLE_H__
#include "emcFEMtuple.h"
#endif
#include <cstdio>

/** (OLD) Stores gain information for several FEMs.
    \deprecated
@ingroup oldemccalib
 */

class emcGains : public emcFEMtuple
{
 public:

  ///
  virtual const char* GetCategory(void) const { return "Gains"; } 

  ///
  void writeDataToFile(FILE * fp, const PHTimeStamp& tStart) ;

} ;


#endif   //  __emcGains__ 


