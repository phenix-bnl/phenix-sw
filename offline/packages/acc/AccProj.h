
#ifndef __ACCPROJ_H_
#define __ACCPROJ_H_

#include "Acc.h"

class AccProj 
{
 public:

  AccProj() {}
  virtual ~AccProj() {}

  int getBoxIDfromXYZ(const float pc2x, const float pc2y, const float pc2z, 
		      const float pc3x, const float pc3y, const float pc3z, const int boxconfig) const;

  int getHitIDfromXYZ(const float pc2x, const float pc2y, const float pc2z, 
		      const float pc3x, const float pc3y, const float pc3z) const;

  int getHitConfig(const int boxid_0, const int boxid_1, const int boxid_2, const int boxid_3, const int hitid) const;
  
};

#endif
