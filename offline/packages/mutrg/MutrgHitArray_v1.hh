#ifndef __MUTRGHITARRAY_V1__
#define __MUTRGHITARRAY_V1__

#include "MutrgHitArray.hh"

class MutrgHitArray_v1 : public MutrgHitArray{
public:
  MutrgHitArray_v1(const char *mutrg_hit_class="");
  virtual ~MutrgHitArray_v1(void);
  MutrgHitArray* Create(const char *mutrg_hit_class="");

  ClassDef(MutrgHitArray_v1,1)
};

#endif /* __MUTRGHITARRAY__ */
