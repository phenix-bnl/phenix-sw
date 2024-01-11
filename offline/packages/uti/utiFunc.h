#ifndef __UTIFUNC_H__
#define __UTIFUNC_H__

#include <math.h>

class utiFunc {

public:
  utiFunc();
  ~utiFunc();
  float pc3MatchSDphi(float mom);
  float pc3MatchSDzps(float mom);
  float tofMatchSDphi(float mom);
  float tofMatchSDzps(float mom);
  float emcMatchSDphi(float mom);
  float emcMatchSDzps(float mom);
  int   tofMass2Pid(float mom, float ms2, float sel, float vet);

};
#endif 

