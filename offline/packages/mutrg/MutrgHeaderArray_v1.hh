#ifndef __MUTRGHEADERARRAY_V1__
#define __MUTRGHEADERARRAY_V1__

#include "MutrgHeaderArray.hh"

class MutrgHeaderArray_v1 : public MutrgHeaderArray{
public:
  MutrgHeaderArray_v1(const char *mutrg_header_class="");
  virtual ~MutrgHeaderArray_v1(void){;}
  virtual MutrgHeaderArray* Create(const char *mutrg_header_class="");

  ClassDef(MutrgHeaderArray_v1,1)
};

#endif /* __MUTRGHEADERARRAY_V1__ */
