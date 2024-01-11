#ifndef __EMCCLUSTERAUXINFO_H__
#define __EMCCLUSTERAUXINFO_H__
#include "PHObject.h"

class emcClusterAuxInfo : public PHObject
{
 public:

  virtual float getLocalChi2() { return 0;};
  virtual float getLocalEcore() { return 0;};
  virtual float getLocalx() { return 0;};
  virtual float getLocaly() { return 0;};

 private:
  ClassDef(emcClusterAuxInfo,1);
  
};

#endif



