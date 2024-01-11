#ifndef __MUTRGTRKARRAY_V1__
#define __MUTRGTRKARRAY_V1__

#include "MutrgTrkArray.hh"

class MutrgTrkArray_v1 : public MutrgTrkArray{
public:
  MutrgTrkArray_v1(const char *mutrg_trk_class="");
  virtual ~MutrgTrkArray_v1(void);
  MutrgTrkArray* Create(void){return new MutrgTrkArray_v1();}

protected:
  ClassDef(MutrgTrkArray_v1,1)
};

#endif /* __MUTRGTRKARRAY__ */
