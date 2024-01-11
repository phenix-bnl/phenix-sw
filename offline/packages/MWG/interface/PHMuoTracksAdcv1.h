#ifndef __PHMUOTRACKSADCV1__
#define __PHMUOTRACKSADCV1__

#include "PHMuoTracksAdc.h"

class PHMuoTracksAdcv1 : public PHMuoTracksAdc{
public:
  PHMuoTracksAdcv1(const char *content_class="");
  virtual ~PHMuoTracksAdcv1(void);
  virtual PHMuoTracksAdc* Create(const char *content_class="");

  ClassDef(PHMuoTracksAdcv1,1)
};

#endif /* __PHMUOTRACKSADCV1__ */
